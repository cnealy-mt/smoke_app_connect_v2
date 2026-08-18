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
library(slider)   # Keep for 24-hr sliding averages
library(terra)    # Keep for vect() and extract()
library(tidyr)
library(tigris)

if (!pingr::is_online()) stop("No internet connection.")

#-----------------------Update Date-----------------------------
update_date <- Sys.Date() 

#----------------------Counties for terra mapping--------------------------
# This is explicitly required by your script's terra::extract(mt_v, ...) call
mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")
mt_v <- vect(mt_counties)

#-----------------------Folder Helper-----------------------
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) fs::dir_create(path)
}

#-----------------------Run Update-------------------------------
cat("🔄 Running AirNow-only update...\n")
source("update_scripts/helpers.R")
source("update_scripts/get_AirNow_data.R")
cat("✅ AirNow-only update complete at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
