source("update_scripts/helpers.R")

# Ensure folders exist
ensure_dir("data/trend")

# Dates
tomorrow_run <- as.Date(update_date)
today_run <- as.Date(update_date) - 1
yesterday_run <- as.Date(compare_date)

# AQI breaks
breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)

# Helper function to safely read and process RDS files
load_county_avg <- function(date_val) {
  file_path <- paste0("data/county_24hr_avg/", date_val, "_county_24hr_avg.rds")
  if (file.exists(file_path)) {
    readRDS(file_path) %>%
      select(county, MASSDEN) %>%
      mutate(AQI_HRRR = cut(MASSDEN, breaks = breaks, labels = FALSE, right = TRUE))
  } else {
    message(paste("Missing file:", file_path))
    return(NULL)
  }
}

# Load data safely
tomorrow_avg  <- load_county_avg(tomorrow_run)
today_avg     <- load_county_avg(today_run)
yesterday_avg <- load_county_avg(yesterday_run)

# 1-day trend
if (!is.null(tomorrow_avg) && !is.null(today_avg)) {
  trend_1day <- left_join(today_avg, tomorrow_avg, by = "county", suffix = c("_today", "_tomorrow")) %>%
    mutate(AQI_HRRR_trend = AQI_HRRR_tomorrow - AQI_HRRR_today) %>%
    select(county, AQI_HRRR_trend)
  
  saveRDS(trend_1day, "data/trend/county_trend_1day.rds")
  print("Saved 1-day trend.")
} else {
  message("Skipping 1-day trend due to missing data.")
}

# 2-day trend
if (!is.null(tomorrow_avg) && !is.null(yesterday_avg)) {
  trend_2day <- left_join(yesterday_avg, tomorrow_avg, by = "county", suffix = c("_yesterday", "_tomorrow")) %>%
    mutate(AQI_HRRR_trend = AQI_HRRR_tomorrow - AQI_HRRR_yesterday) %>%
    select(county, AQI_HRRR_trend)
  
  saveRDS(trend_2day, "data/trend/county_trend_2day.rds")
  print("Saved 2-day trend.")
} else {
  message("Skipping 2-day trend due to missing data.")
}


#------------------------Raster Trends (tomorrow vs. yesterday and today)---------------------
tomorrow_filter <- format(as.Date(tomorrow_run) + 1, "%Y-%m-%d")
today_filter <- format(as.Date(today_run) + 1, "%Y-%m-%d")
yesterday_filter <- format(as.Date(yesterday_run) + 1, "%Y-%m-%d")

for (this_var_name in c(vars$var_name, "VENT_RATE")) {
  
  folder_path <- paste0("data//", this_var_name) 
  print(folder_path)
  
  # Try to safely load each raster
  tomorrow_rast <- tryCatch(rast(paste0(folder_path, "//", this_var_name, "_", tomorrow_run, ".tif")), 
                            error = function(e) {
                              cat(glue("Warning: Missing tomorrow_rast for {this_var_name} ({tomorrow_run})\n"))
                              return(NULL)
                            })
  
  today_rast <- tryCatch(rast(paste0(folder_path, "//", this_var_name, "_", today_run, ".tif")), 
                         error = function(e) {
                           cat(glue("Warning: Missing today_rast for {this_var_name} ({today_run})\n"))
                           return(NULL)
                         })
  
  yesterday_rast <- tryCatch(rast(paste0(folder_path, "//", this_var_name, "_", yesterday_run, ".tif")), 
                             error = function(e) {
                               cat(glue("Warning: Missing yesterday_rast for {this_var_name} ({yesterday_run})\n"))
                               return(NULL)
                             })
  
  # Proceed only if all three rasters are successfully loaded
  if (!is.null(tomorrow_rast) & !is.null(today_rast) & !is.null(yesterday_rast)) {
    
    filter_layers <- function(rast_obj, pattern, label) {
      layer_names <- names(rast_obj)
      layers_to_keep <- grepl(pattern, layer_names)
      
      if (sum(layers_to_keep) == 0) {
        cat(glue("Warning: No matching layers found in {label} raster.\n"))
        return(NULL)
      } else {
        return(rast_obj[[layers_to_keep]])
      }
    }
    
    tomorrow_rast <- filter_layers(tomorrow_rast, tomorrow_filter, "tomorrow")
    today_rast    <- filter_layers(today_rast, today_filter, "today")
    yesterday_rast <- filter_layers(yesterday_rast, yesterday_filter, "yesterday")
    
    # Optional: check the layer names in the subset
    names(tomorrow_rast)
    
    if (is.null(tomorrow_rast) | is.null(today_rast) | is.null(yesterday_rast)) {
      cat(glue("Skipping {this_var_name} due to empty layer subset(s).\n"))
      next  # Skip to the next variable
    }
    
    # Calculate Averages
    tomorrow_avg_rast <- mean(tomorrow_rast, na.rm = TRUE)
    today_avg_rast <- mean(today_rast, na.rm = TRUE)
    yesterday_avg_rast <- mean(yesterday_rast, na.rm = TRUE)
    
    trend_1day_rast <- tomorrow_avg_rast - today_avg_rast
    trend_2day_rast <- tomorrow_avg_rast - yesterday_avg_rast
    
    # Save processed rasters
    writeRaster(trend_1day_rast, paste0("data//trend//", this_var_name, "_1day_trend.tif"), overwrite = TRUE)
    writeRaster(trend_2day_rast, paste0("data//trend//", this_var_name, "_2day_trend.tif"), overwrite = TRUE)
    
    cat(glue("Successfully processed trend rasters for {this_var_name}\n"))
    
  } else {
    cat(glue("Skipping {this_var_name} trend calculation due to missing input rasters.\n"))
    
    # Define file paths for old trend rasters
    trend_1day_path <- paste0("data//trend//", this_var_name, "_1day_trend.tif")
    trend_2day_path <- paste0("data//trend//", this_var_name, "_2day_trend.tif")
    
    # Check if files exist before attempting deletion
    if (file.exists(trend_1day_path)) {
      file.remove(trend_1day_path)
      cat(glue("Deleted old {trend_1day_path} due to missing input rasters.\n"))
    }
    
    if (file.exists(trend_2day_path)) {
      file.remove(trend_2day_path)
      cat(glue("Deleted old {trend_2day_path} due to missing input rasters.\n"))
    }
  }
}

