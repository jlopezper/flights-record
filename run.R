# restore packages snapshot
renv::restore()

# load libraries
library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(yaml)
library(telegram)
library(dotenv)
source("get_data.R")

# load env variables
load_dot_env(file = ".env")

# set bot
bot <- TGBot$new(token = Sys.getenv("BOT_TOKEN"))

# set DB connection
tryCatch(
  {
    source("set_db_conn.R")
  },
  error = function(e) {
    bot$sendMessage(text = "[ERROR] - DB connection fail", chat_id = "743837742")
  }
)

# configuration
config <- read_yaml("config.yml")
airports <- config$airports
init_date <- seq(Sys.Date(), by = "month", length.out = config$months)
end_date <- init_date + months(1)
comb <-
  expand.grid(
    origin = airports,
    destination = airports,
    init_date = init_date,
    end_date = end_date,
    stringsAsFactors = FALSE
  ) %>%
  filter(origin != destination, init_date != end_date) %>%
  mutate(diff_months = interval(init_date, end_date) %/% months(1)) %>%
  filter(diff_months == 1)


# retrieve all flights
flights <-
  tryCatch(
    {
      purrr::pmap(
        .l = list(
          x = comb$origin,
          y = comb$destination,
          z = as.character(format(comb$init_date, "%d/%m/%Y")),
          h = as.character(format(comb$end_date, "%d/%m/%Y"))
        ),
        .f = function(x, y, z, h) {
          safe_get_data(
            origin = x,
            destination = y,
            init_date = z,
            end_date = h
          )
        }
      ) %>%
        map("result") %>%
        compact() %>%
        bind_rows()
    },
    error = function(e) {
      bot$sendMessage(text = "[ERROR] - Failed while retrieving flights data", chat_id = "743837742")
    }
  )


# customize fields
flights <-
  tryCatch(
    {
      customize_data(flights)
    },
    error = function(e) {
      bot$sendMessage(text = "[ERROR] - Failed while customising fields", chat_id = "743837742")
    }
  )

# checks
tryCatch(
  {
    check_data_format(flights)
  },
  error = function(e) {
    bot$sendMessage(text = "[ERROR] - Checks failed", chat_id = "743837742")
  }
)


# Append data in DB
tryCatch(
  {
    dbWriteTable(
      conn = con,
      name = "flights_historical",
      value = flights,
      append = TRUE,
      row.names = FALSE,
      overwrite = FALSE
    )
  },
  error = function(e) {
    bot$sendMessage(text = "[ERROR] - Failed while writing in DB", chat_id = "743837742")
  }
)
dbDisconnect(con)

# Send success message
bot$sendMessage(text = "[SUCCESS] - Successfully completed process", chat_id = "743837742")