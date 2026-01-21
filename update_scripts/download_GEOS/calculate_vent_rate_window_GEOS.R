#------------------------Calculate Vent Rate WINDOW - GEOS---------------------

source("update_scripts/helpers.R")

# Build the expected name of the in-memory stack
stack_name <- paste0("GEOS_VENT_RATE_fine_stack")

# Try to load from memory safely
rast <- if (exists(stack_name, envir = .GlobalEnv)) {
  get(stack_name, envir = .GlobalEnv)
} else {
  cat(glue("Warning: Missing VENT RATE raster for {tomorrow_run}. Skipping processing.\n"))
  NULL
}


n_layers <- nlyr(rast)  # should be 240 for GEOS vent rate data
layer_index <- 1:n_layers

# Each layer timestamp
layer_datetime <- local_runtime_GEOS + hours(layer_index)

# Build reference table
layer_ref <- tibble(
  layer = layer_index,
  datetime = layer_datetime,
  date = as.Date(layer_datetime, tz = "America/Denver"),
  lead_time = as.integer(difftime(date, as.Date(local_runtime_GEOS), units = "days"))
)

process_vent_window <- function(rast, update_date, lead_time, layers_to_keep) {
  
  filtered_rast <- rast[[layers_to_keep]]
  
  valid_count_rast <- app(filtered_rast, fun = function(x) sum(!is.na(x)))
  marginal_rast    <- app(filtered_rast, fun = function(x) sum(x > 2350, na.rm = TRUE))
  good_rast        <- app(filtered_rast, fun = function(x) sum(x > 4700, na.rm = TRUE))
  
  marginal_rast[valid_count_rast < 1] <- NA
  good_rast[valid_count_rast < 1]     <- NA
  
  # Ensure folder exists and remove old files once per var
  var_path <- glue("data/GEOS/VENT_WINDOW")
  ensure_dir(var_path)
  old_files <- fs::dir_ls(var_path, regexp = "\\.tif$")
  old_files <- old_files[!grepl(glue("^{update_date}"), basename(old_files))]
  if (length(old_files) > 0) fs::file_delete(old_files)
  
  writeRaster(marginal_rast,
              paste0("data/GEOS/VENT_WINDOW/", update_date, "_marginal_rast_lead", lead_time, ".tif"),
              overwrite = TRUE)
  writeRaster(good_rast,
              paste0("data/GEOS/VENT_WINDOW/", update_date, "_good_rast_lead", lead_time, ".tif"),
              overwrite = TRUE)
  
  cat(glue::glue("Successfully processed VENT RATE WINDOW for {update_date}, lead_time {lead_time}\n"))
}


if (!is.null(rast)) {
  # Find unique lead times
  unique_leads <- sort(unique(layer_ref$lead_time))
  
  # Loop through each lead_time group
  for (lt in unique_leads) {
    layers_to_keep <- layer_ref$layer[layer_ref$lead_time == lt]
    
    # Check minimum layer count
    if (length(layers_to_keep) < 16) {
      cat(glue::glue("Skipping lead_time {lt}: only {length(layers_to_keep)} hours available\n"))
      next
    }
    
    process_vent_window(
      rast           = GEOS_VENT_RATE_fine_stack,
      update_date    = update_date,
      lead_time      = lt,
      layers_to_keep = layers_to_keep
    )
  }
} else {
  cat(glue("VENT WINDOW processing skipped for {tomorrow_run} due to missing raster file.\n"))
}





