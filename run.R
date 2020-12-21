# restore packages snapshot
renv::restore()

# load libraries
library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(yaml)
source("get_data.R")
source("set_db_conn.R")

# configuration
config <- read_yaml('config.yml')
airports <- config$airports
init_date <- seq(Sys.Date(), by = "month", length.out = config$months)
end_date <- init_date + months(1)
comb <-
  expand.grid(origin = airports,
              destination = airports,
              init_date = init_date, 
              end_date = end_date,
              stringsAsFactors = FALSE) %>% 
  filter(origin != destination, init_date != end_date) %>% 
  mutate(diff_months = interval(init_date, end_date) %/% months(1)) %>% 
  filter(diff_months == 1)


# retrieve all flights
flights <-
  purrr::pmap(.l = list(x = comb$origin,
                        y = comb$destination, 
                        z = as.character(format(comb$init_date, "%d/%m/%Y")),
                        h = as.character(format(comb$end_date, "%d/%m/%Y"))),
              .f = function(x, y, z, h) {safe_get_data(origin = x, 
                                                       destination = y, 
                                                       init_date = z, 
                                                       end_date = h)}) %>% 
  map('result') %>% 
  compact() %>% 
  bind_rows()


# customize fields
flights <- customize_data(flights) 

# checks 
check_data_format(flights)


# Append data in DB
dbWriteTable(conn = con,
             name = "flights_historical",
             value = flights, 
             append = TRUE,
             row.names = FALSE,
             overwrite = FALSE)
dbDisconnect(con)
