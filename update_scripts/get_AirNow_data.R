source("update_scripts/helpers.R")

# Ensure folders exist
ensure_dir("data/AirNow")

# today <- as.Date(update_date) # old way of defining based on update_date
# start_date <- today - 2
AirNow <- readRDS("data/AirNow/AirNow.rds")

# 1. Find the most recent date_gmt
most_recent <- max(AirNow$date_gmt, na.rm = TRUE)

# 2. Start time = most recent + 1 hour
start_time <- most_recent - hours(2)

# 3. End time = current UTC time, truncated to hour
end_time <- floor_date(with_tz(Sys.time(), "UTC"), "hour")

# Format strings for API
start_str <- format(start_time, "%Y-%m-%dT%H")
end_str   <- format(end_time, "%Y-%m-%dT%H")

# Construct URL
url <- paste0(
  "https://www.airnowapi.org/aq/data/?startDate=", start_str,
  "&endDate=", end_str,
  "&parameters=PM25,PM10&BBOX=-116.202774,44.045890,-103.722305,49.229925",
  "&dataType=C&format=text/csv&verbose=1&monitorType=0&includerawconcentrations=0",
  "&API_KEY=4A314159-4658-4690-8CE9-F716E5EABC20"
)
# Print URL to verify
cat(url)

col_names <- c("latitude", "longitude", "date_gmt", "parameter", "sample_measurement", 
               "units_of_measure", "site_name", "monitoring_agency", "AQSID", "Full_AQSID")

# Use tryCatch around GET call
tryCatch({
  response <- GET(url, timeout(30))  # Add timeout to prevent hanging
  
  if (status_code(response) == 200) {
    csv_data <- content(response, "text")
    
    air_quality_data <- read_csv(csv_data, col_names = FALSE)
    
    col_names <- c("latitude", "longitude", "date_gmt", "parameter", "sample_measurement", 
                   "units_of_measure", "site_name", "monitoring_agency", "AQSID", "Full_AQSID")
    colnames(air_quality_data) <- col_names
    
    AirNow_update <- air_quality_data %>%
      mutate(
        local_time = with_tz(date_gmt, tzone = "America/Denver"),
        AQSID = as.character(AQSID),
        Full_AQSID = as.character(Full_AQSID),
        country_code = substr(Full_AQSID, 1, 3),
        state_code   = substr(Full_AQSID, 4, 5),
        county_code  = substr(Full_AQSID, 6, 8),
        site_number  = substr(Full_AQSID, 9, 12)
      ) %>%
      filter(country_code == "840", state_code == "30")
    
    print("AirNow data retrieved successfully.")
    
    #------------------------------Remove rows where sample_measurement is less than -900-------------------------
    AirNow_update <- AirNow_update %>%
      filter(sample_measurement >= -900 & parameter == "PM2.5") 
    
    #------------------------------Append Data Update-------------------------
    
    AirNow_appended <- AirNow %>%
      mutate(source_flag = 0) %>%                      # old = 0
      bind_rows(AirNow_update %>% mutate(source_flag = 1)) %>%  # new = 1
      group_by(site_name, date_gmt) %>%
      slice_max(order_by = source_flag, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      dplyr::select(-source_flag) %>%
      filter(local_time >= Sys.time() - days(3))
    
    saveRDS(AirNow_appended, paste0("data//AirNow//AirNow.rds"))
    
    
    #---------------------------Calculate running average------------------------------------
    AirNow_avg <- AirNow_appended %>%
      arrange(AQSID, local_time) %>%
      group_by(AQSID) %>%
      mutate(
        sample_measurement_24hr_avg = slide_period_dbl(
          .x = sample_measurement,
          .i = date_gmt,                  # Timestamp column
          .period = "hour",               # Sliding by the hour
          .every = 1,                     # Every hour
          .before = 23,                   # Look back 23 previous hours + current = 24 hours
          .complete = TRUE,
          .f = ~mean(.x, na.rm = TRUE)
        )
      ) %>%
      ungroup()
    
    site_coords <- AirNow_avg %>%
      dplyr::select(site_name, latitude, longitude) %>%
      dplyr::distinct()
    site_vect <- vect(site_coords, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    counties_with_sites <- terra::extract(mt_v, site_vect)
    sites_and_counties <- cbind(site_coords, counties_with_sites[,-1]) %>%
      rename(county = NAME) %>%
      dplyr::select(site_name, county)
    
    AirNow_avg <- AirNow_avg %>%
      left_join(sites_and_counties)
    
    
    saveRDS(AirNow_avg, paste0("data//AirNow//AirNow_running_avg.rds"))
    
  } else {
    message(paste("Failed to fetch data. Status code:", status_code(response)))
  }
}, error = function(e) {
  message("GET request failed with error: ", e$message)
})

# Merge New AirNow data to hourly_model_performance
AirNow <- readRDS("data/AirNow/AirNow.rds")
hourly_model_performance <- readRDS("data/model_performance/hourly_model_performance.rds")

#--------------------------Append Most Recent AirNow Update-------------------------
hourly_model_performance <- hourly_model_performance %>%
  left_join(
    AirNow %>% dplyr::select(site_name, local_time, sample_measurement),
    by = c("site_name", "local_time")
  ) %>%
  mutate(
    # If there's a sample_measurement value, use it; otherwise keep existing airnow_obs
    airnow_obs = if_else(!is.na(sample_measurement), sample_measurement, airnow_obs)
  ) %>%
  dplyr::select(-sample_measurement)

#---------------------------Calculate Running 24-hr Averages---------------------------
hourly_model_performance <- hourly_model_performance %>%
  group_by(site_name) %>%
  arrange(local_time, .by_group = TRUE) %>%
  mutate(
    across(
      airnow_obs,
      ~ slide_dbl(.x, mean, .before = 23, .complete = TRUE, na.rm = TRUE),
      .names = "24hr_avg_{.col}"
    )
  ) %>%
  ungroup()

saveRDS(hourly_model_performance, "data/model_performance/hourly_model_performance.rds")

