source("update_scripts/helpers.R")

# Define expected output paths
hourly_path <- paste0("data//county_hrly_avg//", update_date, "_county_hrly_avg.rds")
daily_path <- paste0("data//county_24hr_avg//", update_date, "_county_24hr_avg_lead0.rds")

# Check if either file exists
if (file_exists(hourly_path) && file_exists(daily_path)) {
  cat(glue("⏩ Output already exists for {update_date}, skipping calculation.\n"))
} else {
  # Try running the main script logic
  tryCatch({
    cat(glue("▶ Running calculations for {update_date}\n"))
    
    # Initialize the output data frame
    county_hourly_avg <- NULL
    
    var_loop <- c(as.character(vars$var_name))
    
    # Loop over each variable
    for (this_var_name in var_loop) {
      
      # Build the name of the stack object (e.g., "MASSDEN_stack")
      stack_name <- paste0(this_var_name, "_stack")
      
      # Check if that object exists in memory
      if (exists(stack_name, envir = .GlobalEnv)) {
        message(glue::glue("Using preloaded stack: {stack_name}"))
        
        # Get the actual raster stack
        stack <- get(stack_name, envir = .GlobalEnv)
        
        # Calculate County Hourly Avg
        temp_df <- lapply(seq_len(nlyr(stack)), function(j) {
          r <- stack[[j]]
          extracted <- terra::extract(r, mt_v, fun = mean, na.rm = TRUE)
          
          data.frame(
            county = mt_counties$NAME[extracted$ID],
            fcst_hour = j - 1, # -1 because RRFS data starts with 00z analysis hour
            time_local = local_runtime + hours(j) - hours(1), # ditto
            value = extracted[, 2]
          )
        }) %>% bind_rows()
        
        # Rename 'value' to this_var_name
        names(temp_df)[names(temp_df) == "value"] <- this_var_name
        
        # Merge into the main data frame
        if (is.null(county_hourly_avg)) {
          county_hourly_avg <- temp_df
        } else {
          county_hourly_avg <- dplyr::full_join(
            county_hourly_avg, temp_df,
            by = c("county", "fcst_hour", "time_local")
          )
        }
        
      } else {
        message(glue::glue("No preloaded stack found for {this_var_name}, adding empty column"))
        
        # If main df exists, just add NA column
        if (!is.null(county_hourly_avg)) {
          county_hourly_avg[[this_var_name]] <- NA_real_
        }
      }
    }
    
    
    
    #--------------------------Calculate County 24-hr Avg-------------------------
    # Build reference table (copied from calculate_VENT_window.R)
    leadtime_ref <- tibble(
      datetime = county_hourly_avg$time_local,
      date = as.Date(county_hourly_avg$time_local, tz = "America/Denver"),
      lead_time = as.integer(difftime(date, as.Date(local_runtime), units = "days"))
    ) %>%
      distinct()
    
    unique_leads <- sort(unique(leadtime_ref$lead_time))
    
    #--------------------------Add AQI Data-------------------------
    
    # Define your breakpoints and category labels
    breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)
    labels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                "Unhealthy", "Very Unhealthy", "Hazardous")
    
    # Loop through each lead_time group
    for (lt in unique_leads) {
      rows_to_keep <- leadtime_ref %>%
        filter(lead_time == lt)
      
      # Check minimum layer count
      if (nrow(rows_to_keep) < 16) {
        cat(glue::glue("Skipping lead_time {lt}: only {nrow(rows_to_keep)} hours available\n"))
        next
      }
      
      
      vent_rate_max_df <- county_hourly_avg %>%
        filter(time_local %in% rows_to_keep$datetime) %>%
        mutate(date = as.Date(time_local, tz = "America/Denver")) %>%
        group_by(county, date) %>%
        summarise(
          VENT_RATE_max = max(VENT_RATE, na.rm = TRUE),
          .groups = "drop"
        )
      
      county_24hr_avg <- county_hourly_avg %>%
        filter(time_local %in% rows_to_keep$datetime) %>%
        mutate(date = as.Date(time_local, tz = "America/Denver")) %>%
        group_by(county, date) %>%
        summarise(
          across(all_of(var_loop), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        left_join(vent_rate_max_df, by = c("county", "date"))
      
      county_24hr_avg <- county_24hr_avg %>%
        mutate(AQI_category = cut(MASSDEN,
                                  breaks = breaks,
                                  labels = labels,
                                  right = TRUE, include.lowest = TRUE))
      
      # Ensure folder exists and remove old files once per var
      var_path <- glue("data/county_24hr_avg/")
      ensure_dir(var_path)
      old_files <- fs::dir_ls(var_path, regexp = "\\.rds$")
      old_files <- old_files[!grepl(glue("^{update_date}"), basename(old_files))]
      if (length(old_files) > 0) fs::file_delete(old_files)
      
      write_rds(county_24hr_avg, paste0("data//county_24hr_avg//", update_date, "_county_24hr_avg_lead", lt, ".rds"))
    }
      
    county_hourly_avg <- county_hourly_avg %>%
      mutate(AQI_category = cut(MASSDEN,
                                breaks = breaks,
                                labels = labels,
                                right = TRUE, include.lowest = TRUE))

    # Ensure folder exists and remove old files once per var
    var_path <- glue("data/county_hrly_avg/")
    ensure_dir(var_path)
    old_files <- fs::dir_ls(var_path, regexp = "\\.rds$")
    old_files <- old_files[!grepl(glue("^{update_date}"), basename(old_files))]
    if (length(old_files) > 0) fs::file_delete(old_files)
    
    write_rds(county_hourly_avg, paste0("data//county_hrly_avg//", update_date, "_county_hrly_avg.rds"))

    
    cat(glue("✅ Finished writing outputs for {update_date}\n"))
  }, error = function(e) {
    cat(glue("❌ Error processing {update_date}: {e$message}\n"))
  })
}


