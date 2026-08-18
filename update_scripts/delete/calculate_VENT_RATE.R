#Produce VENT RATE

# Proceed only if both files exist
if (!is.null(WIND_1hr_max_fcst_stack) & !is.null(HPBL_stack)) {
  this_var_name <- "VENT_RATE"
  
  # Define output folder path
  folder_path <- "www/VENT_RATE"
  
  VENT_RATE_stack <- WIND_1hr_max_fcst_stack * HPBL_stack
  stack <- VENT_RATE_stack
 
  
  # Create folder if it doesn't exist
  if (!fs::dir_exists(folder_path)) {
    fs::dir_create(folder_path)
  }
  
  # Save result
  source("update_scripts/write_hourly_png.R")
  
  cat(glue("Successfully processed VENT RATE for {update_date}\n"))
} else {
  cat(glue("VENT RATE processing skipped for {update_date} due to missing input files.\n"))
}

