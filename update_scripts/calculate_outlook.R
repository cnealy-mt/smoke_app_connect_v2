source("update_scripts/helpers.R")

# ----------------------------
# Variables and stack types
# ----------------------------
max_vars <- c("MASSDEN", "TMP", "WIND_1hr_max_fcst", "VENT_RATE")
avg_vars <- c("MASSDEN")
acc_vars <- c("PRATE")

all_vars <- list(
  max = max_vars,
  avg = avg_vars,
  acc = acc_vars
)

# ----------------------------
# Main loop over variables
# ----------------------------
for (type in names(all_vars)) {
  for (var in all_vars[[type]]) {
    
    stack_name <- paste0(var, "_stack")
    
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
    layer_datetime <- local_runtime + hours(layer_index) - hours(1)
    
    layer_ref <- tibble(
      layer = layer_index,
      datetime = layer_datetime,
      date = as.Date(layer_datetime, tz = "America/Denver"),
      lead_time = as.integer(
        difftime(date, as.Date(local_runtime), units = "days")
      )
    )
    
    unique_leads <- sort(unique(layer_ref$lead_time))
    
    # ----------------------------
    # Prepare output folder
    # ----------------------------
    var_path <- glue("data/{var}")
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
      
      if (length(layers_to_keep) < 16) {
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
        avg = mean(r_sub),
        acc = sum(r_sub)
      )
      
      # ----------------------------
      # Write GeoTIFF
      # ----------------------------
      out_file <- glue("{var_path}/{update_date}_{var}_{type}_lead{lt}.tif")
      writeRaster(out_rast, out_file, overwrite = TRUE)
      
      cat(glue("Saved rectilinear raster: {out_file}\n"))
    }
  }
}
