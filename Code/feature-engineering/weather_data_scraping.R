library(tidyverse)
library(httr)
library(jsonlite)

setwd("/Users/ronankelly/Desktop/")

df_coordinates <- readr::read_csv('stadium_coordinates.csv')
df_games <- readr::read_csv('games.csv')

# Converting dates to date format
df_games <- df_games  %>%
  mutate(
    TimeStartGame = lubridate::parse_date_time(TimeStartGame, orders = '%m/%d/%Y %H:%M'),
    TimeEndGame = lubridate::parse_date_time(TimeEndGame, orders = '%m/%d/%Y %H:%M')
  )

# List of game_ids
game_ids_of_interest <- unique(df_games$game_id)

# Filtering the dataframe to only include games with these game_ids
df_weather_fetch <- df_games %>%
  filter(game_id %in% game_ids_of_interest) %>%
  mutate(first_date = substr(TimeStartGame - lubridate::days(1), 1, 10),
         last_date = substr(TimeEndGame + lubridate::days(1), 1, 10)) %>%
  inner_join(df_coordinates, by = c("StadiumName" = "StadiumName")) %>%
  select(game_id, Longitude, Latitude, first_date, last_date) %>%
  distinct()

# Initialize an empty dataframe for storing weather results
df_weather_raw_method1 <- data.frame()

# API Key for Open-Meteo 
for(i in seq(1, nrow(df_weather_fetch))) {
  
  # Tracking which iteration of game
  print(paste0(i, ' / ', nrow(df_weather_fetch)))
  
  # Selecting row
  row <- df_weather_fetch[i,]
  
  # Query to get historical weather data for given latitude and longitude
  query_weather <- paste0('https://api.open-meteo.com/v1/forecast?latitude=', 
                          row$Latitude, '&longitude=', row$Longitude, 
                          '&hourly=temperature_2m,precipitation,windspeed_10m&start=', 
                          row$first_date, 'T00:00:00Z&end=', row$last_date, 'T23:59:59Z')
  
  # Assuming result is missing to start
  query_weather_content <- ""
  
  consecutive_calls <- 0
  
  # While result is missing
  while(query_weather_content == "") {
    
    print(paste0("Querying weather: ", query_weather))
    # Response of query
    query_weather_response <- httr::GET(url = query_weather)
    
    # Prevent more than 1 call per second 
    Sys.sleep(1)
    
    print(paste0("Weather API Response: ", query_weather_response$status_code))
    
    # Print the full response text to debug
    query_weather_content <- httr::content(query_weather_response, as = 'text')
    print(paste0("Raw Weather Response: ", query_weather_content))
    
    # Adding to consecutive calls
    consecutive_calls <- consecutive_calls + 1
    
    # At this point we assume API limit is reached for the day
    if(consecutive_calls > 5){
      
      # Saving csv of progress
      write.csv(x = df_weather_raw_method1, file = 'weather_raw.csv', row.names = F)
      
      # Breaking code when out of calls for the day
      stop("Out of API calls for the day")
      
    }
    
  }
  
  # Cleaned data of query
  query_weather_data = fromJSON(query_weather_content)$hourly %>% as.data.frame()
  
  # If data exists, process it
  if(nrow(query_weather_data) > 0){
    df_weather_raw_method1 <- bind_rows(df_weather_raw_method1,
                                        cbind(row,
                                              query_weather_data))
  }
  
}

# Saving the final data
write.csv(df_weather_raw_method1, 'weather_raw_final.csv', row.names = FALSE)

# Rerun for 10 matches that didnt scrape because API limit hit

