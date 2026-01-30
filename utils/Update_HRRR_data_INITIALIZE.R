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
#----------------------Counties for calculate_county_hourly_avg & get_AirNow_data--------------------------
mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")

mt_v <- vect(mt_counties)

#-----------------------Folder Helper-----------------------
# ensures data folders exist first time app update is run
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) fs::dir_create(path)
}

#flags <- readRDS("C:/R Projects (dev)/HRRR_tools/misc/wf_flags/wf_flags_PM25_all_MT.rds")
# Filter the data
#target_dates <- as.Date("2025-06-06")
target_dates <- seq.Date(from = as.Date("2025-06-10"), to = as.Date("2025-06-11"), by = "day")
# target_dates <- flags %>%
#   filter(
#     qualifier != "",                                    # Non-empty qualifier
#     date_local >= as.Date("2024-09-27"),               # Within last 5 years
#     date_local <= Sys.Date()                           # Not in the future
#   ) %>%
#   distinct(date_local) %>%                             # Keep unique dates
#   arrange(date_local) %>%                              # Sort chronologically
#   pull(date_local)


# Step 2: Add 2-day lookback for each date
all_dates <- unique(sort(c(
  target_dates,
  target_dates - days(1),
  target_dates - days(2)
)))

# Step 3: Create groups for contiguous date "islands"
group_id <- cumsum(c(1, diff(all_dates) > 1))

# Step 4: Create the update_dates_df
update_dates_df <- data.frame(update_date = all_dates) %>%
  mutate(group = group_id) %>%
  group_by(group) %>%
  mutate(index = row_number()) %>%
  ungroup()

print(update_dates_df)


# Define script groups
early_scripts <- c(
  "update_scripts/HRRR_download.R",
  "update_scripts/calculate_VENT_RATE.R",
  "update_scripts/calculate_county_hourly_avg.R",
  "update_scripts/calculate_VENT_window.R"
)

later_scripts <- c(
  "update_scripts/archive_data.R",
  "update_scripts/get_AirNow_data.R",
  "update_scripts/model_performance.R",
  "update_scripts/calculate_trends.R"
  #"update_scripts/get_fire_data.R"
)

# Iterate through each index and associated date
for (i in seq_len(nrow(update_dates_df))) {
  update_date <- update_dates_df$update_date[i]
  index <- update_dates_df$index[i]
  
  cat("🔁 Processing iteration index:", index, " — Update date:", update_date, "\n")
  
  compare_date <- update_date - days(2)
  
  # Timing
  start_time <- Sys.time()
  cat("✅ Update started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

  # Run early scripts
  for (script in early_scripts) {
    cat("▶ Running:", script, "\n")
    source(script)
  }

  # Only run later scripts if index ≥ 3 within the contiguous group
  if (index >= 3) {
    cat("▶ Running later scripts (since it's the 3rd day or later in group)\n")
    for (script in later_scripts) {
      cat("▶ Running:", script, "\n")
      source(script)
    }
  }
  
  # Delete old files in data/ folders
  subdirs <- dir("data", full.names = TRUE, recursive = FALSE)
  
  for (subdir in subdirs) {
    # Skip if it's the 'data/trend' directory
    if (basename(subdir) == "trend") next
    
    if (!dir_exists(subdir)) next  # Skip if not a directory
    
    # Get all files (not folders) in this subdirectory
    files <- dir(subdir, full.names = TRUE, recursive = FALSE)
    files <- files[file.info(files)$isdir == FALSE]
    
    if (length(files) > 8) {
      # Sort by modification time (newest first)
      files_to_keep <- files %>%
        tibble::tibble(path = ., mtime = file.info(.)$mtime) %>%
        arrange(desc(mtime)) %>%
        slice_head(n = 8) %>%
        pull(path)
      
      # Files to delete = all files not in the 5 most recent
      files_to_delete <- setdiff(files, files_to_keep)
      
      for (file_path in files_to_delete) {
        cat("🗑️ Deleting old file:", file_path, "\n")
        file.remove(file_path)
      }
    }
  }
  
  # End timing
  end_time <- Sys.time()
  cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Time difference
  time_diff <- end_time - start_time
  cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n\n")
}

