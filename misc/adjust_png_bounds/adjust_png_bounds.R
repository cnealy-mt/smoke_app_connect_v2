#12/30/2025 This script supercedes "write_png.R" and should be used to tweak png map layer placement
library(terra)
library(ggplot2)
library(dplyr)
library(scales)
library(raster)
library(png)
library(base64enc)
library(leaflet)
library(htmlwidgets)

# -------------------------
# Color Scale
# -------------------------
MASSDEN_breaks <- c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25, 30, 40, 60, 100, 200, Inf)
MASSDEN_colors <- c("transparent", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")

# -------------------------
# Load Raster and Calculate Bounds
# -------------------------
r <- rast("data/temp/test/test.tif")
r <- rast("data/TMP/2026-01-02_TMP_max_lead0.tif")
ext_r <- ext(trim(r))
bounds <- list(
  unname(c(ext_r$ymin, ext_r$xmin)),  # SW (lat, lon)
  unname(c(ext_r$ymax, ext_r$xmax))   # NE (lat, lon)
)
print(bounds)

# -------------------------
# Rasterize 1 layer for Leaflet
# -------------------------
r1 <- r[[1]]  # first layer
r1_raster <- raster(r1)  # for Leaflet


# -------------------------
# Create Single Test PNG
# -------------------------
# Trim layer to remove padding (optional)
r_trimmed <- trim(r[[1]])

# Convert to data frame
df <- as.data.frame(r_trimmed, xy = TRUE)
names(df)[3] <- "value"

# Aspect ratio based on raster extent
aspect_ratio <- (max(df$y) - min(df$y)) / (max(df$x) - min(df$x))

lat_lines <- c(45, 49)      # horizontal
lon_lines <- c(-116, -104)  # vertical

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
  ) +
  # Add latitude lines
  geom_hline(yintercept = lat_lines, color = "red", linetype = "dashed", size = 0.5) +
  # Add longitude lines
  geom_vline(xintercept = lon_lines, color = "blue", linetype = "dashed", size = 0.5)
gg

ggsave(
  filename = "misc/adjust_png_bounds/test_bounds.png",
  plot = gg,
  bg = "transparent",
  dpi = 300,
  width = 6,
  height = 6 * (
    (bounds[[2]][1] - bounds[[1]][1]) /
      (bounds[[2]][2] - bounds[[1]][2])
  )
)



# tmp_png is the path of your PNG
png_path <- "misc/adjust_png_bounds/test_bounds.png"

# -------------------------
# Encode PNG for Leaflet (bypasses need for app to be running with images in www/ folder)
# -------------------------
png_base64 <- base64enc::dataURI(file = png_path, mime = "image/png")

# Original bounds
bounds
# [[1]] SW
# [1] 42.50495 -122.51513
# [[2]] NE
# [1] 52.24368 -103.15003

# -------------------------
# Customize Bounds
# -------------------------
montana_bounds <- list(
  SW = c(bounds[[1]][1] - 0.54, bounds[[1]][2] - 0.00),  # move south & west
  NE = c(bounds[[2]][1] - 0.38, bounds[[2]][2] + 0.00)   # move north & east
)
saveRDS(montana_bounds, "modules/utils/png_bounds.rds") # IMPORTANT: use this line to save new bounds; app automatically uses the new bounds

# -------------------------
# Leaflet map
# -------------------------
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  
  # Overlay PNG using base64
  htmlwidgets::onRender(sprintf("
    function(el, x) {
      var map = this;
      var bounds = [[%f, %f], [%f, %f]];
      var img = L.imageOverlay('%s', bounds, {opacity: 0.5});
      img.addTo(map);
    }
  ",
                                montana_bounds$SW[1], montana_bounds$SW[2],
                                montana_bounds$NE[1], montana_bounds$NE[2],
                                png_base64
  )) %>%
  
  # Add original raster semi-transparent
  addRasterImage(r1_raster, opacity = 0.4) %>%
  
  # Add lat/lon reference lines
  addPolylines(lng = c(xmin(r1_raster), xmax(r1_raster)), lat = c(49, 49), color = "red", weight = 2, label = "49° N") %>%
  addPolylines(lng = c(xmin(r1_raster), xmax(r1_raster)), lat = c(45, 45), color = "red", weight = 2, label = "45° N") %>%
  addPolylines(lng = c(-104, -104), lat = c(ymin(r1_raster), ymax(r1_raster)), color = "blue", weight = 2, label = "104° W") %>%
  addPolylines(lng = c(-116, -116), lat = c(ymin(r1_raster), ymax(r1_raster)), color = "blue", weight = 2, label = "116° W") %>%
  
  # Zoom to raster
  fitBounds(lng1 = xmin(r1_raster), lat1 = ymin(r1_raster), lng2 = xmax(r1_raster), lat2 = ymax(r1_raster))



