# --------------------------------------------
# Run backfill for a range of dates
# --------------------------------------------
start_date <- as.Date("2026-01-12")
end_date   <- as.Date("2026-01-14")

date_seq <- seq.Date(start_date, end_date, by = "day")

# Define script list ONCE
scripts <- c(
  # Define Python vars
  "update_scripts/set_env_vars.R",
  
  # RRFS processing
  "update_scripts/download_RRFS/subset_rrfs.R",
  "update_scripts/netcdf_to_geotiff.R",
  "update_scripts/save_hourly_pngs.R",
  "update_scripts/calculate_outlook.R",
  "update_scripts/calculate_VENT_window.R",
  "update_scripts/calculate_county_hourly_avg.R",
  "update_scripts/get_AirNow_data.R",
  "update_scripts/model_performance.R",
  "update_scripts/get_fire_data.R",
  
  # GEOS-FP processing
  "update_scripts/download_GEOS/download_GEOS.R",
  "update_scripts/download_GEOS/crop_GEOS.R",
  "update_scripts/download_GEOS/calculate_vent_rate_GEOS.R",
  "update_scripts/download_GEOS/smooth.R",
  "update_scripts/download_GEOS/calculate_outlook_GEOS.R",
  "update_scripts/download_GEOS/save_hourly_pngs_GEOS.R",
  "update_scripts/download_GEOS/calculate_vent_rate_window_GEOS.R",
  "update_scripts/download_GEOS/calculate_county_hourly_avg_GEOS.R",
  "update_scripts/download_GEOS/model_performance_GEOS.R"
)

# --------------------------------------------
# Loop over dates
# --------------------------------------------
for (update_date in date_seq) {
  update_date <- as.Date(update_date, origin = "1970-01-01")
  
  if (!pingr::is_online()) stop("No internet connection.")
  
  #------------------------UTC OFFSET-------------------------------
  montana_time <- as.POSIXlt(Sys.time(), tz = "America/Denver")
  offset_hours <- montana_time$gmtoff / 3600
  
  #------------------------Model Time----------------------------
  model_runtime <- ymd_hm(paste(update_date, "06:00"), tz = "UTC")
  local_runtime <- with_tz(model_runtime, tzone = "America/Denver") 
  # when switching to RRFS and grabbing 0-hour analysis layer, be sure 
  # to update the layer_datetime logic so that fcst hour 0 isn't layer 1 
  # will need to change in write_hourly_png.R (for i in 1:nlyr(stack)), calculate_VENT_window.R (layer_datetime), and calculate_outlook.R (layer_datetime) 
  
  #----------------------Counties for calculate_county_hourly_avg & get_AirNow_data--------------------------
  mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
  mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")
  
  mt_v <- vect(mt_counties)
  
  #-----------------------Folder Helper-----------------------
  # ensures data folders exist first time app update is run
  ensure_dir <- function(path) {
    if (!fs::dir_exists(path)) fs::dir_create(path)
  }
  
  #----------------------Cleanup Helper----------------------
  clean_folder <- function(path) {
    files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)
    
    keep <- grepl(update_date, files) | grepl(update_str, files) #removes files that don't have either current update_date (YYYY-MM-DD) or update_str (YYYYMMDD)
    remove <- files[!keep]
    
    if (length(remove)) file.remove(remove)
    invisible(length(remove))
  }
  
  #------------------------UPDATE SCRIPT-------------------------------
  # Function to get the latest update date from the folder
  get_latest_update_date <- function(dir_path = "data/county_24hr_avg") {
    files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
    if (length(files) == 0) return(as.Date("1900-01-01")) # Fallback for empty dir
    dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
    max(dates, na.rm = TRUE)
  }
  
  assign("update_date", update_date, envir = .GlobalEnv)
  
  cat("\n============================================\n")
  cat("📅 Processing update_date:", format(update_date, "%Y-%m-%d"), "\n")
  cat("============================================\n")
  
  start_time <- Sys.time()
  cat("✅ Update started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  GEOS_OK <- TRUE  # reset per date
  
  for (script in scripts) {
    
    # Skip remaining GEOS scripts if GEOS failed
    if (!GEOS_OK && grepl("download_GEOS", script)) {
      cat("⏭️ Skipping:", script, "(GEOS disabled)\n")
      next
    }
    
    cat("▶ Running:", script, "\n")
    
    if (script == "update_scripts/download_GEOS/download_GEOS.R") {
      
      GEOS_OK <- tryCatch(
        source(script)$value,
        error = function(e) {
          message("❌ GEOS download failed for ", update_date)
          FALSE
        }
      )
      
      if (!isTRUE(GEOS_OK)) {
        cat("⛔ GEOS pipeline disabled for this date\n")
      }
      
    } else {
      source(script)
    }
  }
  
  end_time <- Sys.time()
  cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  time_diff <- end_time - start_time
  cat("⏱️ Elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n")
  
  #rm(list = ls())
}
