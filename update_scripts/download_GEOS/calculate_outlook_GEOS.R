#calculate_outlook_GEOS.R

source("update_scripts/helpers.R")

# ----------------------------
# Variables and stack types
# ----------------------------
max_vars <- c("GEOS_ocsmass", "GEOS_VENT_RATE")
avg_vars <- c("GEOS_ocsmass")

all_vars <- list(
  max = max_vars,
  avg = avg_vars
)

#switches GEOS naming to naming convention used throughout app logic
file_var_name <- function(var) {
  switch(
    var,
    GEOS_ocsmass   = "MASSDEN",
    GEOS_VENT_RATE = "VENT_RATE",
    var   # default: unchanged
  )
}

#GEOS-specific times
model_runtime_GEOS <- ymd_hm(paste(update_date, "00:00"), tz = "UTC")
local_runtime_GEOS <- with_tz(model_runtime_GEOS, tzone = "America/Denver") 

# ----------------------------
# Main loop over variables
# ----------------------------
for (type in names(all_vars)) {
  for (var in all_vars[[type]]) {
    
    out_var <- file_var_name(var)
    
    stack_name <- paste0(var, "_fine_stack")
    
    if (!exists(stack_name, envir = .GlobalEnv)) {
      cat(glue("Warning: Missing in-memory stack for {var}. Skipping.\n"))
      next
    }
    
    # ----------------------------
    # Pull georeferenced stack
    # ----------------------------
    stack <- get(stack_name, envir = .GlobalEnv)
    
    n_time <- nlyr(stack)
    if (n_time == 0) {
      cat(glue("Warning: {var} stack has no layers. Skipping.\n"))
      next
    }
    
    # ----------------------------
    # Reference table for layers / lead times
    # ----------------------------
    layer_index <- seq_len(n_time)
    if (out_var == "VENT_RATE") {
      layer_datetime <- local_runtime_GEOS + hours(layer_index) #00z model run 1st layer is 00-01z for VENT_RATE
    } else {
      layer_datetime <- local_runtime_GEOS + hours(layer_index*3) - hours(3) #00z model run 1st layer is 21z-24z of previous day for MASSDEN
    }
    
    layer_ref <- tibble(
      layer = layer_index,
      datetime = layer_datetime,
      date = as.Date(layer_datetime, tz = "America/Denver"),
      lead_time = as.integer(
        difftime(date, as.Date(local_runtime_GEOS), units = "days")
      )
    )
    
    unique_leads <- sort(unique(layer_ref$lead_time))
    
    # ----------------------------
    # Prepare output folder
    # ----------------------------
    var_path <- glue("data/GEOS/{out_var}")
    if (!dir_exists(var_path)) dir_create(var_path)
    
    # Remove old files EXCEPT those from today's update_date
    old_files <- dir_ls(var_path, regexp = "\\.tif$")
    files_to_delete <- old_files[!grepl(update_date, basename(old_files))]
    if (length(files_to_delete) > 0) {
      file_delete(files_to_delete)
    }
    
    # ----------------------------
    # Process each lead time
    # ----------------------------
    for (lt in unique_leads) {
      
      layers_to_keep <- layer_ref$layer[layer_ref$lead_time == lt]
      
      min_layers <- switch(
        out_var,
        MASSDEN   = 5,
        VENT_RATE = 16,
        5  # default / fallback
      )
      
      if (length(layers_to_keep) < min_layers) {
        cat(glue(
          "Skipping {var} lead_time {lt}: only {length(layers_to_keep)} layers\n"
        ))
        next
      }
      
      r_sub <- stack[[layers_to_keep]]
      
      # ----------------------------
      # Compute statistic
      # ----------------------------
      out_rast <- switch(
        type,
        max = max(r_sub),
        avg = mean(r_sub)
      )
      
      # ----------------------------
      # Write GeoTIFF
      # ----------------------------
      out_file <- glue("{var_path}/{update_date}_{out_var}_{type}_lead{lt}.tif")
      writeRaster(out_rast, out_file, overwrite = TRUE)
      
      cat(glue("Saved rectilinear raster: {out_file}\n"))
    }
  }
}

#r <- rast("data/GEOS/GEOS_VENT_RATE/2026-01-04_GEOS_VENT_RATE_max_lead9.tif")
#plot(r)


