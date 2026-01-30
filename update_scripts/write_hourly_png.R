source("update_scripts/helpers.R")
# Step 7: Save hourly PNGs
source("modules/utils/map_module_utils.R")

palette_name <- paste0(this_var_name, "_palette")
palette_func <- get(palette_name)

# ---- Remove old PNGs before processing ----
all_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.png$")
# keep only files matching today's update_date
old_files <- all_files[!grepl(update_date, all_files)]
if (length(old_files) > 0) file.remove(old_files)

# ---- Loop through layers ----
for (i in 1:nlyr(stack)) {
  
  # Build filename for this layer
  png_file <- sprintf("%s/%s_%02d.png", folder_path, update_date, i)
  
  # Skip if file already exists
  if (file.exists(png_file)) {
    message(sprintf("Skipping (exists): %s", png_file))
    next
  }
  
  # Trim layer
  stack_trimmed <- trim(stack[[i]])
  
  # Convert to data frame
  df <- as.data.frame(stack_trimmed, xy = TRUE)
  names(df)[3] <- "value"
  
  # Aspect ratio based on raster extent
  aspect_ratio <- (max(df$y) - min(df$y)) / (max(df$x) - min(df$x))
  
  # Map colors dynamically
  df$fill_color <- palette_func(df$value)
  
  gg <- ggplot(df, aes(x = x, y = y, fill = fill_color)) +
    geom_raster() +
    scale_fill_identity() +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "cm"),
      legend.position = "none"
    )
  
  # Save PNG for this layer
  ggsave(
    png_file,
    plot = gg,
    width = 6,
    height = 6 * aspect_ratio,
    dpi = 300,
    bg = "transparent"
  )
  
  message(sprintf("Saved: %s", png_file))
}


# WRITE BOUNDS IF THEY CHANGE

# r <- rast("data/MASSDEN/MASSDEN_2025-07-24.tif")
# 
# ext_r <- ext(trim(r))
# bounds <- list(
#   unname(c(ext_r$ymin, ext_r$xmin)),  # SW (lat, lon)
#   unname(c(ext_r$ymax, ext_r$xmax))   # NE (lat, lon)
# )
# print(bounds)
# saveRDS(bounds, "modules/utils/png_bounds.rds")
