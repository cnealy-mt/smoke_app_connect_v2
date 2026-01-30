#---------------------------------Get Data-----------------------------------------

library(reticulate)

required_packages <- c(
  "s3fs", "xarray", "metpy", "boto3",
  "numcodecs", "numpy", "dataclasses",
  "datetime", "zarr"
)

py_require(required_packages)

cat("✅ Using Python at:", py_config()$python, "\n")
#print(py_list_packages())


# Pass year, month, and day to Python
update_date_py <- as.Date(update_date) # we want this as UTC date since that's how the model files are structured (check update_date in UPDATE_HRRR_APP.R)

# Safe variable names
year_val <- as.integer(format(update_date_py, "%Y"))
month_val <- as.integer(format(update_date_py, "%m"))
day_val <- as.integer(format(update_date_py, "%d"))


vars <- tibble::tibble(
  var_level = c("8m_above_ground", "10m_above_ground", "2m_above_ground", "2m_above_ground", "surface", "surface", "surface"),
  var_name = c("MASSDEN", "WIND_1hr_max_fcst", "TMP", "RH", "GUST", "HPBL", "PRATE")
)


# Then loop through them
for (i in seq_len(nrow(vars))) {
  # Pull the var_level and var_name for this iteration
  this_var_level <- vars$var_level[i]
  this_var_name <- vars$var_name[i]
  
  # Try running py_run_string(), catch errors
  tryCatch({
    # Save to a multi-layer GeoTIFF
    file_date <- format(update_date_py, "%Y-%m-%d")  # Formats as "2024-09-09"
    folder_path <- paste0("www/", this_var_name)
    
    fs::dir_create(folder_path)    # recreate empty folder 
    
    py_run_string(glue("
import dataclasses
import datetime

@dataclasses.dataclass
class ZarrId:
    run_hour: datetime.datetime
    level_type: str
    var_level: str
    var_name: str
    model_type: str
        
    def format_chunk_id(self, chunk_id):
        if self.model_type == 'fcst': 
            # Extra id part since forecasts have an additional (time) dimension
            return '0.' + str(chunk_id)
        else:
            return chunk_id

# Receive year, month, and day from R
run_hour = datetime.datetime({year_val}, {month_val}, {day_val}, 12)

zarr_id = ZarrId(
    run_hour=run_hour,
    level_type='sfc',
    var_level='{this_var_level}',
    var_name='{this_var_name}',
    model_type='fcst'
)
"))
  
  # If you want to check the object in Python, you can print or return it
  py_run_string("print(zarr_id)")
  
  # Run the Python script
  source_python("update_scripts//HRRR_download.py")
  
  #---------------------------------Process Data to Raster-----------------------------------------
  
  
  
  #---------------Layer Names
  
  # Step 1: Extract run time and forecast hours
  run_time <- zarr_id$run_hour              # Should be a POSIXct or datetime object
  forecast_hours <- py$py_time + 1             # Vector of hours: 0, 1, 2, ..., 47
  
  #---------------Make Raster Stack
  
  # Pull x and y from Python (these are LCC-projected coordinates)
  x_vals <- py$py_x  # 1D vector along x (450)
  y_vals <- py$py_y  # 1D vector along y (300)
  
  # Step 5: Create raster layers and assign names
  rasters <- lapply(seq_along(forecast_hours), function(i) {
    mat <- py$py_variable[i, , ]
    
    # Extract year from the update_date_py string
    update_year <- as.integer(substr(update_date_py, 1, 4))
    
    # Apply conditional transformations based on var_name
    if (this_var_name == "MASSDEN" && update_year != 2021) {
      mat <- mat * 1e9  # kg/m³ to µg/m³
    } else if (this_var_name == "TMP") {
      mat <- (mat - 273.15) * (9/5) + 32  # Kelvin to Fahrenheit
    } else if (this_var_name == "PRATE") {
      mat <- mat * 141.732  # kg/m²/s to inches/hour
    }
    
    
    # Flip the matrix vertically
    mat <- mat[nrow(mat):1, ]
    
    y_flipped <- rev(y_vals)
    
    r <- rast(mat,
              extent = ext(min(x_vals), max(x_vals), min(y_flipped), max(y_flipped)),
              crs = "+proj=lcc +lat_1=38.5 +lat_2=38.5 +lat_0=38.5 +lon_0=-97.5 +a=6371229 +b=6371229 +units=m +no_defs")
    
    project(r, "EPSG:4326")
  })
  
  # Step 6: Combine and label
  stack <- rast(rasters)
  names(stack) <- forecast_hours
  
  assign(paste0(this_var_name, "_stack"), stack)
  
  # Step 7: Save hourly PNGs
  source("update_scripts/write_hourly_png.R")
  
  cat(glue("Successfully processed {this_var_name} for {file_date}\n"))
    
  }, error = function(e) {
    # Print a warning if it fails
    cat(glue("Warning: Failed to process {this_var_name} ({this_var_level}) — {e$message}\n"))
  })
}

# Preview
#plot(stack[[1]])

# #-------------------------------------Mapping-------------------------------------
# # Define breakpoints and corresponding colors
# breaks <- c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25, 30, 40, 60, 100, 200, Inf)
# colors <- c("white", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")
# 
# # Create color function
# color_fun <- colorBin(palette = colors, bins = breaks, na.color = "transparent")
# 
# # Get the name of the current raster layer
# layer_label <- names(stack)[10]  # Change index if using a different layer
# 
# # Leaflet map with label
# leaflet() %>%
#   addTiles() %>%
#   addRasterImage(stack[[10]], colors = color_fun, opacity = 0.5) %>%
#   addLegend(pal = color_fun,
#             values = values(stack[[10]]),
#             title = "Concentration (µg/m³)",
#             position = "bottomright") %>%
#   addControl(
#     html = paste0("<div style='background: rgba(255,255,255,0.8); 
#                              padding: 4px 8px; 
#                              border-radius: 4px;
#                              font-size: 14px;'>
#                       <strong>Forecast Time:</strong> ", layer_label, "
#                   </div>"),
#     position = "topright"
#   )




