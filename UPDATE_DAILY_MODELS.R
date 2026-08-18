library(dplyr)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(leaflet)
library(lubridate)
library(pingr)
library(readr)
library(sf)
library(slider)
library(terra)
library(tidyr)
library(tigris)
library(viridis)
library(ggplot2)
library(ncdf4)
library(stars)

if (!pingr::is_online()) stop("No internet connection.")

#-----------------------Update Date-----------------------------
update_date <- Sys.Date() 

#------------------------UTC OFFSET-------------------------------
montana_time <- as.POSIXlt(Sys.time(), tz = "America/Denver")
offset_hours <- montana_time$gmtoff / 3600

#------------------------Model Time----------------------------
model_runtime <- ymd_hm(paste(update_date, "06:00"), tz = "UTC")
local_runtime <- with_tz(model_runtime, tzone = "America/Denver") 

#----------------------Counties Setup--------------------------
mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")
mt_v <- vect(mt_counties)

#-----------------------Folder Helper-----------------------
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) fs::dir_create(path)
}

#----------------------Cleanup Helper----------------------
clean_folder <- function(path) {
  files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)
  keep <- grepl(update_date, files) | grepl(update_str, files)
  remove <- files[!keep]
  if (length(remove)) file.remove(remove)
  invisible(length(remove))
}

#------------------------UPDATE SCRIPT-------------------------------
get_latest_update_date <- function(dir_path = "data/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
  if (length(files) == 0) return(as.Date("1900-01-01"))
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
  max(dates, na.rm = TRUE)
}

get_latest_update_date_GEOS <- function(dir_path = "data/GEOS/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
  if (length(files) == 0) return(as.Date("1900-01-01"))
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
  max(dates, na.rm = TRUE)
}

run_scheduled_update <- function() {
  source("update_scripts/helpers.R")
  
  current_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  current_hour <- as.numeric(format(current_utc, "%H"))
  current_min <- as.numeric(format(current_utc, "%M"))
  
  latest_file_date <- get_latest_update_date()
  latest_file_date_GEOS <- get_latest_update_date_GEOS()
  
  after_cutoff <- current_hour > 11 || (current_hour == 11 && current_min >= 5)
  rrfs_outdated <- latest_file_date < update_date
  geos_outdated <- latest_file_date_GEOS < update_date
  
  cat("⏰ UTC Time:", format(current_utc, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("📂 Latest RRFS date:", format(latest_file_date, "%Y-%m-%d"), "\n")
  cat("📂 Latest GEOS date:", format(latest_file_date_GEOS, "%Y-%m-%d"), "\n")
  cat("📅 Target update_date:", format(update_date, "%Y-%m-%d"), "\n")
  
  # Condition A: Everything is outdated -> Run entire suite
  if (after_cutoff && rrfs_outdated) {
    scripts <- c(
      "update_scripts/set_env_vars.R",
      "update_scripts/download_RRFS/subset_rrfs.R",
      "update_scripts/netcdf_to_geotiff.R",
      "update_scripts/save_hourly_pngs.R",
      "update_scripts/calculate_outlook.R",
      "update_scripts/calculate_VENT_window.R",
      "update_scripts/calculate_county_hourly_avg.R",
      "update_scripts/get_AirNow_data.R",
      "update_scripts/model_performance.R",
      "update_scripts/get_fire_data.R",
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
    
    start_time <- Sys.time()
    cat("✅ Full processing suite started.\n")
    
    for (script in scripts) {
      cat("▶ Running:", script, "\n")
      
      if (script == "update_scripts/download_GEOS/download_GEOS.R") {
        GEOS_OK <- tryCatch(source(script)$value, error = function(e) FALSE)
        if (!isTRUE(GEOS_OK)) {
          cat("⛔ Skipping remaining GEOS scripts due to download failure\n")
          break
        }
      } else if (script == "update_scripts/get_fire_data.R") {
        # --- WRAP FIRE DATA IN A SAFETYN_NET ---
        FIRE_OK <- tryCatch({
          source(script)
          TRUE
        }, error = function(e) {
          cat("⚠️ WARNING: get_fire_data.R failed with spatial geometry error:\n", e$message, "\n")
          cat("⏭️ Skipping fire overlay to save the rest of the model updates...\n")
          FALSE
        })
      } else {
        source(script)
      }
    }
    
    time_diff <- Sys.time() - start_time
    cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n\n")
    
  # Condition B: RRFS is good, but GEOS missed -> Catch up GEOS only
  } else if (after_cutoff && !rrfs_outdated && geos_outdated) {
    scripts <- c(
      "update_scripts/set_env_vars.R",
      "update_scripts/get_AirNow_data.R",
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
    
    start_time <- Sys.time()
    cat("✅ GEOS-only catch-up processing started.\n")
    
    for (script in scripts) {
      cat("▶ Running:", script, "\n")
      
      if (script == "update_scripts/download_GEOS/download_GEOS.R") {
        GEOS_OK <- tryCatch(source(script)$value, error = function(e) FALSE)
        if (!isTRUE(GEOS_OK)) {
          cat("⛔ Skipping remaining GEOS scripts due to download failure\n")
          break
        }
      } else if (script == "update_scripts/get_fire_data.R") {
        # --- WRAP FIRE DATA IN A SAFETYN_NET ---
        FIRE_OK <- tryCatch({
          source(script)
          TRUE
        }, error = function(e) {
          cat("⚠️ WARNING: get_fire_data.R failed with spatial geometry error:\n", e$message, "\n")
          cat("⏭️ Skipping fire overlay to save the rest of the model updates...\n")
          FALSE
        })
      } else {
        source(script)
      }
    }
    
    time_diff <- Sys.time() - start_time
    cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n\n")
    
  } else {
    # If data is already current, exit cleanly without processing
    cat("🎉 Data is already fully up-to-date for today. Nothing to do!\n")
  }
}

run_scheduled_update()
