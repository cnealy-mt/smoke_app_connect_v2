source("update_scripts/helpers.R")
#----------------------------------------------------------------------------
#--------------------------------- CALCULATE WIND ---------------------------
#----------------------------------------------------------------------------
# Proceed only if both wind components exist
if (exists("UGRD_stack") && exists("VGRD_stack")) {
  
  this_var_name <- "WIND_1hr_max_fcst"

  WIND_1hr_max_fcst_stack <- sqrt(UGRD_stack^2 + VGRD_stack^2)
  
  cat(glue(
    "Successfully processed WIND (1-hr max forecast) for {update_date}\n"
  ))
} else {
  cat(glue(
    "WIND processing skipped for {update_date} due to missing UGRD/VGRD stacks.\n"
  ))
}

#----------------------------------------------------------------------------
#--------------------------------- CALCULATE VENT_RATE ---------------------------
#----------------------------------------------------------------------------

# Proceed only if both files exist
if (!is.null(WIND_1hr_max_fcst_stack) & !is.null(HPBL_stack)) {
  this_var_name <- "VENT_RATE"
  
  VENT_RATE_stack <- WIND_1hr_max_fcst_stack * HPBL_stack

  cat(glue("Successfully processed VENT RATE for {update_date}\n"))
} else {
  cat(glue("VENT RATE processing skipped for {update_date} due to missing input files.\n"))
}


#----------------------------------------------------------------------------
#--------------------- WRITE HOURLY PNGs ---------------------------
#----------------------------------------------------------------------------

vars <- tibble::tibble(
  var_name = c("WIND_1hr_max_fcst", "VENT_RATE", "MASSDEN",
               "TMP", "RH", "GUST", "HPBL", "PRATE")
)

#--------------------Convert Units--------------------------
for (i in seq_len(nrow(vars))) {
  
  tryCatch({ 
    
    this_var_name <- vars$var_name[i]
    stack_name <- paste0(this_var_name, "_stack")
    
    # Pull object from memory
    stack <- get(stack_name, envir = .GlobalEnv)
    
    # ----------------------------
    # 3️⃣ Unit conversions
    # ----------------------------
    if (this_var_name == "MASSDEN") {
      stack <- stack * 1e9
    } else if (this_var_name == "TMP") {
      stack <- (stack - 273.15) * (9/5) + 32
    } else if (this_var_name == "PRATE") {
      stack <- stack * 141.732
    }
    
    # Overwrite in memory
    assign(stack_name, stack, envir = .GlobalEnv)
    
    cat(glue("Successfully converted {this_var_name} units for {update_date}\n"))
    
  }, error = function(e) {
    cat(glue(
      "Warning: Failed to process {this_var_name} — {e$message}\n"
    ))
  })
}


#-----------------------write PNGs-------------------
for (i in seq_len(nrow(vars))) {
  
  tryCatch({
    
    this_var_name <- vars$var_name[i]
    stack_name <- paste0(this_var_name, "_stack")
    
    # Pull object from memory
    stack <- get(stack_name, envir = .GlobalEnv)
    
    # Output folder
    folder_path <- file.path("www", this_var_name)
    if (!fs::dir_exists(folder_path)) {
      fs::dir_create(folder_path)
    }
    
    source("update_scripts/write_hourly_png.R")
    
    cat(glue("Successfully processed {this_var_name} for {update_date}\n"))
    
  }, error = function(e) {
    cat(glue(
      "Warning: Failed to process {this_var_name} — {e$message}\n"
    ))
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




