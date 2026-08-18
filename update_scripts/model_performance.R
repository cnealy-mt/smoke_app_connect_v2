source("update_scripts/helpers.R")

# Ensure folders exist
ensure_dir("data/model_performance")


#--------------------------Load AirNow data-------------------------
airnow_path <- paste0("data//AirNow//AirNow.rds")

if (file.exists(airnow_path)) {
  cat(glue("✅ AirNow data found: {airnow_path}\n"))
  
  # Load AirNow data
  AirNow <- readRDS(airnow_path)
  
  # Get monitoring site coordinates
  site_coords <- AirNow %>%
    dplyr::select(site_name, latitude, longitude) %>%
    dplyr::distinct()
  
  site_vect <- vect(site_coords, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  
  #--------------------------Check for Smoke File-------------------------
  
  if (exists("MASSDEN_stack") && !is.null(MASSDEN_stack)) {
    cat("✅ MASSDEN_stack exists in memory and is not NULL\n")
    
    # Extract values
    values <- terra::extract(MASSDEN_stack, site_vect)

    # Add site names back
    values <- cbind(site_coords, values)

    #--------------------------------Restructure--------------------------------
    reshape_smoke_data <- function(values) {
      # Convert to long format and ensure correct timezone handling
      smoke_long <- values %>%
        pivot_longer(
          cols = -c(site_name, latitude, longitude, ID),  
          names_to = "fcst_hour",
          values_to = "smoke_ug_m3"
        ) %>%
        mutate(
          # Extract numeric hour from "hour_XX"
          fcst_hour = as.integer(sub("hour_", "", fcst_hour)),
          
          # Now this works correctly
          local_time = local_runtime + hours(fcst_hour)
        ) %>%
        dplyr::select(-ID)
      
      smoke_long
    }
    
    smoke_data <- reshape_smoke_data(values)
    
    smoke_data <- smoke_data %>%
      mutate(date = as_date(local_time)) %>%
      group_by(site_name, date) %>%
      mutate(
        n_per_day = n()
      ) %>%
      ungroup() %>%
      filter(n_per_day >= 16) %>%
      mutate(
        lead_time = if_else(
          n_per_day >= 16,
          as.integer(difftime(date, min(date), units = "days")), # assigns lead time in days only if there are 16 or more hours of data
          NA_integer_
        )
      )
    
    smoke_data_wide <- smoke_data %>%
      filter(!is.na(lead_time)) %>%
      mutate(lead_col = glue("model_smoke_lead{lead_time}")) %>%
      dplyr::select(site_name, latitude, longitude, local_time, smoke_ug_m3, lead_col) %>%
      pivot_wider(names_from = lead_col, values_from = smoke_ug_m3)
    
    #--------------------------Append New Model Data-------------------------
    # 1. Update Overlapping Forecast Data (i.e., rows with same site_name and date_time)
    hourly_model_performance <- readRDS("data/model_performance/hourly_model_performance.rds")
    
    hourly_model_performance_updated <- bind_rows(hourly_model_performance, smoke_data_wide) %>%
      group_by(site_name, local_time) %>%
      summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop") # uses non-NA value to replace across each column of a row
    
    #--------------------------Append Most Recent AirNow Update-------------------------
    hourly_model_performance_updated <- hourly_model_performance_updated %>%
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
    hourly_model_performance_updated <- hourly_model_performance_updated %>%
      group_by(site_name) %>%
      arrange(local_time, .by_group = TRUE) %>%
      mutate(
        across(
          c(starts_with("model_smoke_lead"), airnow_obs),
          ~ slide_dbl(.x, mean, .before = 23, .complete = TRUE, na.rm = TRUE),
          .names = "24hr_avg_{.col}"
        )
      ) %>%
      ungroup()
    

    
    #-------------------------Save Hourly Model Performance------------------------------
    hourly_model_performance_updated <- hourly_model_performance_updated %>%
      filter(local_time >= (Sys.time() - years(5)))
    
    saveRDS(hourly_model_performance_updated, "data/model_performance/hourly_model_performance.rds")
    
    
    
    
    
    #--------------------------Calculate Daily Model Performance-------------------------
    # THIS NOW OCCURS IN model_performance_GEOS.R BECAUSE GEOS DATA NEEDS TO BE ADDED BEFORE SUMMARIZING
    daily_avg <- hourly_model_performance_updated %>%
      mutate(date = as_date(local_time)) %>%       # extract date from timestamp
      group_by(site_name, date) %>%
      summarise(
        across(
          c(starts_with("model_smoke_lead"), starts_with("GEOS_model_smoke_lead"), airnow_obs),
          ~ mean(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      filter(date < as.Date(update_date))
    
    #--------------------------Add AQI Category Accuracy (daily only)-------------------------
    
    breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)
    
    daily_avg <- daily_avg %>%
      mutate(
        AQI_obs = cut(airnow_obs, breaks = breaks, labels = FALSE, right = FALSE)
      ) %>%
      rowwise() %>% 
      mutate(
        across(
          c(starts_with("model_smoke_lead"), starts_with("GEOS_model_smoke_lead")),
          ~ cut(.x, breaks = breaks, labels = FALSE, right = FALSE),
          .names = "AQI_{.col}"
        ),
        across(
          c(starts_with("model_smoke_lead"), starts_with("GEOS_model_smoke_lead")),
          ~ {
            aqi_model <- cut(.x, breaks = breaks, labels = FALSE, right = FALSE)
            aqi_model - AQI_obs
          },
          .names = "{.col}_accuracy"
        )
      ) %>%
      ungroup()
    
    #--------------------------Save Daily Model Performance-------------------------
    
    saveRDS(daily_avg, "data/model_performance/daily_model_performance.rds")
    
  } else {
    cat("⚠ MASSDEN_stack is missing or NULL\n") 
    cat("⛔ Aborting: Smoke raster must be present to proceed.\n")
  }
  
} else {
  # Handle missing AirNow data
  cat(glue("⛔ Skipping model performance script. AirNow file not found: {airnow_path}\n"))
}
