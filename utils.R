flights %>%
  filter(n_legs == 1, as_date(snapshot_datetime) == today()) %>% 
  group_by(depature_date = lubridate::date(departure_datetime)) %>%
  summarise(n = n_distinct(id)) %>% 
  ggplot(aes(x = depature_date, y = n)) +
  geom_line() + 
  labs(x = 'Departure date',
       y = 'Number of flights',
       title = 'Flights by departure date')
