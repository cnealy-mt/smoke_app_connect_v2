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
update_date <- Sys.Date() #UTC (change to America/Denver if ever need to run updates after on/after 5pm to avoid UTC-local +1 date offset)
#update_date <- as.Date("2026-01-14") #for testing purposes

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

get_latest_update_date_GEOS <- function(dir_path = "data/GEOS/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
  if (length(files) == 0) return(as.Date("1900-01-01")) # Fallback for empty dir
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
  max(dates, na.rm = TRUE)
}

# Main scheduled update function
run_scheduled_update <- function() {
  
  source("update_scripts/helpers.R")
  
  # Current time in UTC
  current_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  current_hour <- as.numeric(format(current_utc, "%H"))
  current_min <- as.numeric(format(current_utc, "%M"))
  
  # Latest data update available
  latest_file_date <- get_latest_update_date()
  latest_file_date_GEOS <- get_latest_update_date_GEOS()
  
  after_cutoff <- current_hour > 11 || (current_hour == 11 && current_min >= 5)
  
  rrfs_outdated <- latest_file_date < update_date
  geos_outdated <- latest_file_date_GEOS < update_date
  
  cat("⏰ UTC Time:", format(current_utc, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("📂 Latest file date in county_24hr_avg:", format(latest_file_date, "%Y-%m-%d"), "\n")
  cat("📅 Today's update_date:", format(update_date, "%Y-%m-%d"), "\n")
  
  # Check condition: after 11:05 UTC and latest data < today
  if (after_cutoff && rrfs_outdated) {
    
    # Define script groups
    scripts <- c(
      #Define Python vars
      "update_scripts/set_env_vars.R",
      #RRFS processing
      "update_scripts/download_RRFS/subset_rrfs.R",
      "update_scripts/netcdf_to_geotiff.R",
      "update_scripts/save_hourly_pngs.R",
      "update_scripts/calculate_outlook.R",
      "update_scripts/calculate_VENT_window.R",
      "update_scripts/calculate_county_hourly_avg.R",
      "update_scripts/get_AirNow_data.R",
      "update_scripts/model_performance.R",
      "update_scripts/get_fire_data.R",
      #GEOS-FP processing
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
    
    # Timing
    start_time <- Sys.time()
    cat("✅ Update started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Run early scripts
    for (script in scripts) {
      cat("▶ Running:", script, "\n")
      if (script == "update_scripts/download_GEOS/download_GEOS.R") {
        GEOS_OK <- tryCatch(
          source(script)$value,
          error = function(e) FALSE
        )
        if (!isTRUE(GEOS_OK)) {
          cat("⛔ Skipping remaining GEOS scripts due to download failure\n")
          break
        }
      } else {
        source(script)
      }
    }
    
    
    # End timing
    end_time <- Sys.time()
    cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Time difference
    time_diff <- end_time - start_time
    cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n\n")
    
  } else if (after_cutoff && !rrfs_outdated && geos_outdated) {
    
    # Define script groups
    scripts <- c(
      #Define Python vars
      "update_scripts/set_env_vars.R",
      #Get AirNow data
      "update_scripts/get_AirNow_data.R",
      #GEOS-FP processing
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
    
    # Timing
    start_time <- Sys.time()
    cat("✅ Update started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Run early scripts
    for (script in scripts) {
      cat("▶ Running:", script, "\n")
      if (script == "update_scripts/download_GEOS/download_GEOS.R") {
        GEOS_OK <- tryCatch(
          source(script)$value,
          error = function(e) FALSE
        )
        if (!isTRUE(GEOS_OK)) {
          cat("⛔ Skipping remaining GEOS scripts due to download failure\n")
          break
        }
      } else {
        source(script)
      }
    }
    
    # End timing
    end_time <- Sys.time()
    cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Time difference
    time_diff <- end_time - start_time
    cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n\n")
    
    
    } else {
    # If conditions not met, run AirNow-only update
    cat("🔄 Running AirNow-only update...\n")
    update_date <- Sys.Date()
    assign("update_date", update_date, envir = .GlobalEnv)
    source("update_scripts/get_AirNow_data.R")
    cat("✅ AirNow-only update complete at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  }
}

# Run the scheduled update
run_scheduled_update()

# #===================================Deploy Changes to GitHub==================================
# library(gert)
# 
# # Ensure working directory is repo root
# setwd("C:/R Projects (dev)/GitHub/smoke_app_connect")
# 
# # Stage all changes
# git_add(".")
# 
# # Check if there are changes to commit
# status <- git_status()
# if (nrow(status) > 0) {
#   # Amend previous commit instead of making a new one
#   git_commit(message = "Automated hourly update", amend = TRUE)
#   
#   # Push (force because we amended history)
#   git_push(remote = "origin", refspec = "refs/heads/main", force = TRUE)
# }


