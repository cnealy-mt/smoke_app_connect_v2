
# # Define your forecast point (example lat/lon)
# forecast_point <- data.frame(
#   lon = -108.5,  # longitude
#   lat = 45.8     # latitude
# )

get_point_fcst <- function(lat, lon) {
  # Create a data frame for the clicked point
  forecast_point <- data.frame(lon = lon, lat = lat)
  
  # Convert to SpatVector (terra)
  forecast_sf <- terra::vect(forecast_point, geom = c("lon", "lat"), crs = "EPSG:4326")  # WGS84
  
  point_hourly_avg <- NULL
  
  var_loop = c("MASSDEN", "WIND_1hr_max_fcst", "TMP", "RH", "PRATE", "GUST", "HPBL", "VENT_RATE")
  
  # Loop over each variable
  for (var in var_loop) {
    
    folder_path <- paste0("data/", var) 
    filename <- fs::path(folder_path, paste0(var, "_", today, ".tif"))
    
    if (fs::file_exists(filename)) {
      message(glue::glue("File exists: loading {var} for {today}"))
      stack <- rast(filename)
      
      # Then extract per hour
      temp_df <- lapply(seq_len(nlyr(stack)), function(j) {
        r <- stack[[j]]
        extracted <- terra::extract(r, forecast_sf)
        
        data.frame(
          fcst_hour = j-1,
          timestamp_MDT = names(stack)[j],
          value = extracted[, 2]
        )
      }) %>% bind_rows()
      
      # Rename 'value' to var
      names(temp_df)[names(temp_df) == "value"] <- var
      
      # Merge into main data frame
      if (is.null(point_hourly_avg)) {
        point_hourly_avg <- temp_df
      } else {
        point_hourly_avg <- dplyr::full_join(point_hourly_avg, temp_df,
                                             by = c("fcst_hour", "timestamp_MDT"))
      }
      
    } else {
      message(glue::glue("File does not exist: adding empty column for {var}"))
      
      # If the main data frame exists, just add a column of NA
      if (!is.null(point_hourly_avg)) {
        point_hourly_avg[[var]] <- NA_real_
      }
    }
  }
  return(point_hourly_avg)
  

}

