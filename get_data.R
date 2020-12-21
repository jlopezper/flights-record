# Get basic response
get_resp <- function(origin, destination, init_date, end_date, attempts = 5, ...) {
  
  print(glue("Reading {origin}{destination} from {init_date} to {end_date}"))
  
  # Build URL
  url <- glue("https://api.skypicker.com/flights?flyFrom={origin}&to={destination}&dateFrom={init_date}&dateTo={end_date}&limit=2000&partner=picky&v=3")
  
  # Get resp
  resp <- httr::GET(url, ...)
  
  if (httr::status_code(resp) == 200) {
    if (httr::http_type(resp) != "application/json") {
      stop("The API returned an unusual format and not a JSON", call. = FALSE)
    }
    resp  
  } else if(attempts == 0) {
    httr::stop_for_status(resp)
  } else {
    message("Can't connect to API. Retrying...")
    Sys.sleep(2)
    get_resp(url, attempts - 1)
    
  }
  
  
}


# Obtain flights from response´
get_flights <- function(origin, destination, init_date, end_date, ...) {
  # Get response
  response <- get_resp(origin, destination, init_date, end_date)
  response <- jsonlite::fromJSON(content(response, "text"))
  
  if(!inherits(response$data, "data.frame")) {
    stop("The data slot is not a data.frame!")
  }
  
  flight_data <- response$data %>% 
    mutate(departure_datetime = as.POSIXct(dTime, origin="1970-01-01"),
           arrival_datetime = as.POSIXct(aTime, origin="1970-01-01"),
           snapshot_datetime = Sys.time())
  # set sleep time between calls
  Sys.sleep(2) 
  
  flight_data
}

# Safe function to control errors
safe_get_data <- safely(get_flights)


# Customise the data
customize_data <- function(flight_data) {
  
  # select only flights with only one carrier
  # also remove duplicates by id, in case they exist
  flight_data <- distinct(flight_data, id, .keep_all = TRUE) %>% 
    filter(pnr_count == 1) 
  
  # unnest data that comes from the route columns
  flight_data_aux <- flight_data %>% 
    unnest(route, names_repair = tidyr_legacy)
  
  # select relevant columns and grab other ones from the nested dataframes
  flight_data <- 
    flight_data %>% 
    select(
      snapshot_datetime,
      id,
      dTime,
      dTimeUTC,
      aTime,
      aTimeUTC,
      departure_datetime,
      arrival_datetime,
      fly_duration,
      origin = flyFrom,
      destination = flyTo,
      origin_city = cityFrom,
      origin_city_code = cityCodeFrom,
      destination_city = cityTo,
      destination_city_code = cityCodeTo,
      distance,
      airlines,
      price,
      pnr_count  
    ) %>% 
    mutate(airlines =  sapply(.$airlines, paste, collapse=", "),
           fly_duration = as.numeric(difftime(arrival_datetime, departure_datetime, units = "mins")),
           n_legs = sapply(strsplit(id, "|", fixed = TRUE), length)) %>% 
    left_join(flight_data_aux[c('id', 'latFrom', 'lngFrom', 'latTo', 'lngTo', 'flight_no', 'flyFrom1', 'flyTo1')],
              by = c('id' = 'id')) %>% 
    rename(
      flyFrom = flyFrom1,
      FlyTo = flyTo1
    )
  
  # return data
  flight_data
  
}

# Check data format before inserting in database
check_data_format <- function(flight_data) {
  
  reference_data <- tibble(
    snapshot_datetime = double(),
    id = character(),
    dTime = integer(),
    dTimeUTC = integer(),
    aTime = integer(),
    aTimeUTC = integer(),
    departure_datetime = double(),
    arrival_datetime = double(),
    fly_duration = numeric(),
    origin = character(),
    destination = character(),
    origin_city = character(),
    origin_city_code = character(),
    destination_city = character(),
    destination_city_code = character(),
    distance = numeric(),
    airlines = character(),
    price = integer(),
    pnr_count = integer(),
    n_legs = integer(),
    latFrom = numeric(),
    lngFrom = numeric(),
    latTo = numeric(),
    lngTo = numeric(),
    flight_no = integer(),
    flyFrom = character(),
    flyTo = character()
  )
  
  
  # compare data types
  comparison <- sapply(reference_data, typeof) == sapply(flight_data, typeof)
  if(!all(comparison)) {
    incorrect_vars <- which(comparison == 0)
    stop(paste0("Variable(s) ", 
                paste(names(incorrect_vars), collapse = ", "), 
                " has/have an incorrect data type"))
  } 
}
