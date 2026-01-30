source("update_scripts/helpers.R")

#-----------STEP 2-------------
# Converts data from native RRFS curvilinear coordinates (stored in .nc files) 
# to rectilinear georeferenced rasters

#---------------------------------Load netCDF RRFS files-----------------------------------------
nc_dir <- "data/temp"

nc_files <- list.files(
  nc_dir,
  pattern = "\\.nc$",
  full.names = TRUE
)

if (length(nc_files) == 0) {
  stop("No NetCDF files found in ", nc_dir)
}

for (f in nc_files) {
  
  fname <- basename(f)
  prefix <- sub("_.*$", "", fname)
  obj_name <- paste0(prefix, "_stack")
  
  # ----------------------------
  # 1️⃣ Open NetCDF safely
  # ----------------------------
  nc <- tryCatch(nc_open(f), error = function(e) {
    warning("Failed to open ", fname, ": ", e$message)
    return(NULL)
  })
  if (is.null(nc)) next
  
  # Robust variable selection
  var_name <- if("data_value" %in% names(nc$var)) "data_value" else names(nc$var)[1]
  
  # Extract all data while the file is OPEN
  z_stack <- ncvar_get(nc, var_name, collapse_degen = FALSE)
  lat     <- ncvar_get(nc, "latitude")
  lon     <- ncvar_get(nc, "longitude")
  
  # Determine number of time layers from the dimensions object
  forecast_hours <- nc$dim$forecast_hour$len %||% dim(z_stack)[3]
  
  # NOW close the file
  nc_close(nc)
  
  # ----------------------------
  # 2️⃣ Determine number of time layers safely
  # ----------------------------
  if (is.na(forecast_hours) || forecast_hours == 0) {
    warning("Skipping file ", fname, ": forecast_hour dimension missing or zero")
    next
  }
  
  n_time <- forecast_hours
  
  # ----------------------------
  # 3️⃣ Prepare target raster grid
  # ----------------------------
  target_grid <- rast(
    xmin = -124.75, xmax = -103.5,
    ymin = 39, ymax = 53.5,
    resolution = 0.03,
    crs = "EPSG:4326"
  )
  
  # ----------------------------
  # 4️⃣ Loop through layers to rasterize & smooth
  # ----------------------------
  r_list <- vector("list", n_time)
  
  for (t in seq_len(n_time)) {
    vals <- z_stack[,,t]
    
    # Skip layer if all NA/Inf
    if (all(!is.finite(vals))) {
      warning(sprintf("Skipping time step %d in %s: all values NA/Inf", t, fname))
      next
    }
    
    pts <- data.frame(
      lon = as.vector(lon),
      lat = as.vector(lat),
      value = as.vector(vals)
    )
    pts <- pts[is.finite(pts$value), ]
    
    # Skip if no valid points
    if (nrow(pts) == 0) {
      warning(sprintf("Skipping time step %d in %s: no valid points", t, fname))
      next
    }
    
    pts_vect <- vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326")
    
    r_layer <- rasterize(pts_vect, target_grid, field = "value", fun = mean)
    
    # Smooth with 3x3 focal
    r_layer <- focal(r_layer, w = matrix(1,3,3), fun = mean, na.rm = TRUE)
    
    r_list[[t]] <- r_layer
  }
  
  # ----------------------------
  # 5️⃣ Combine layers into stack safely
  # ----------------------------
  r_list <- Filter(Negate(is.null), r_list)  # remove skipped layers
  if (length(r_list) == 0) {
    warning("No raster layers created for ", fname)
    next
  }
  
  r_smooth_stack <- rast(r_list)
  names(r_smooth_stack) <- paste0("hour_", seq_len(length(r_list)) - 1)
  
  assign(obj_name, r_smooth_stack, envir = .GlobalEnv)
  
  cat(sprintf("✅ Loaded %s → %s (%d layers)\n", fname, obj_name, length(r_list)))
}
