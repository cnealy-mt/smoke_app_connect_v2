library(terra)
library(ggplot2)
library(dplyr)

# --- Load raster ---
r <- rast("data/MASSDEN/MASSDEN_2025-07-24.tif")

MASSDEN_breaks <- c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25, 30, 40, 60, 100, 200, Inf)
MASSDEN_colors <- c("transparent", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")

# --- Ensure output folder exists ---
dir.create("www/png_layers", showWarnings = FALSE)

# --- Loop through each layer ---
for (i in 1:nlyr(r)) {
  # Trim layer to remove padding (optional)
  r_trimmed <- trim(r[[i]])
  
  # Convert to data frame
  df <- as.data.frame(r_trimmed, xy = TRUE)
  names(df)[3] <- "value"
  
  # Aspect ratio based on raster extent
  aspect_ratio <- (max(df$y) - min(df$y)) / (max(df$x) - min(df$x))
  
  # Build ggplot
  gg <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(colors = MASSDEN_colors) +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "cm"),
      legend.position = "none"
    )
  
  # Save PNG for this layer
  png_file <- sprintf("www/png_layers/layer_%02d.png", i)
  ggsave(
    png_file,
    plot = gg,
    width = 6,
    height = 6 * aspect_ratio,
    dpi = 100,
    bg = "transparent"
  )
  
  message(sprintf("Saved: %s", png_file))
}

# Extent (useful for Leaflet bounds)
ext_r <- ext(trim(r))
bounds <- list(
  unname(c(ext_r$ymin, ext_r$xmin)),  # SW (lat, lon)
  unname(c(ext_r$ymax, ext_r$xmax))   # NE (lat, lon)
)
print(bounds)
saveRDS(bounds, "modules/utils/png_bounds.rds")
