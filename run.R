library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(yaml)

config <- read_yaml('config.yml')
airports <- config$airports
init_date <- seq(Sys.Date(), by = "month", length.out = 9)
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

tst2 <-
  purrr::pmap(.l = list(x = comb$origin,
                        y = comb$destination, 
                        z = as.character(format(comb$init_date, "%d/%m/%Y")),
                        h = as.character(format(comb$end_date, "%d/%m/%Y"))),
              .f = function(x, y, z, h) {safe_get_data(origin = x, 
                                                       destination = y, 
                                                       init_date = z, 
                                                       end_date = h)})


tst3 <- tst2 %>% 
  map('result') %>% 
  compact() %>% 
  bind_rows()


tst3 <- customize_data(tst3) 

check_data_format(tst3)

## 

tst3 %>%
  filter(n_legs == 1) %>% 
  group_by(depature_date = lubridate::date(departure_datetime)) %>%
  summarise(n = n_distinct(id)) %>% 
  ggplot(aes(x = depature_date, y = n)) +
  geom_line() + 
  labs(x = 'Departure date',
       y = 'Number of flights',
       title = 'Flights by departure date')
