# test netcdf to geotiff workflow; feeds testing of PNG bounds too
library(ncdf4)
library(sf)
library(leaflet)

# ----------------------------
# 1️⃣ Open TMP NetCDF
# ----------------------------
nc_file <- "data/temp/TMP_subset_stack_20260102.nc"

nc <- nc_open(nc_file)

# Identify variable name (replace if needed)
var_name <- names(nc$var)[1]  # main variable

# Read full stack: lon x lat x time
z_stack <- ncvar_get(nc, var_name)
lat <- ncvar_get(nc, "latitude")   # 2D
lon <- ncvar_get(nc, "longitude")  # 2D

nc_close(nc)

# ----------------------------
# 2️⃣ Compute 24-hr statistics
# ----------------------------
z_max <- apply(z_stack, c(1,2), max, na.rm = TRUE)
z_avg <- apply(z_stack, c(1,2), mean, na.rm = TRUE)
z_sum <- apply(z_stack, c(1,2), sum, na.rm = TRUE)  # optional

# ----------------------------
# 3️⃣ Write a curvilinear NetCDF
# ----------------------------
# Output file
out_file <- "data/temp/test.nc"

# Dimensions
dim_x <- ncdim_def("x", "grid", 1:ncol(z_max))
dim_y <- ncdim_def("y", "grid", 1:nrow(z_max))

# Define variables at creation
var_TMP <- ncvar_def("TMP_max", "K", dim = list(dim_x, dim_y),
                     longname = "24-hr Max Temperature",
                     missval = NaN, prec = "double")
var_lat <- ncvar_def("lat", "degrees_north", dim = list(dim_x, dim_y),
                     longname = "latitude", prec = "double")
var_lon <- ncvar_def("lon", "degrees_east", dim = list(dim_x, dim_y),
                     longname = "longitude", prec = "double")

# Create NetCDF with all three variables
nc_out <- nc_create(out_file, vars = list(var_TMP, var_lat, var_lon))

# Write data
ncvar_put(nc_out, var_TMP, z_max)
ncvar_put(nc_out, var_lat, lat)
ncvar_put(nc_out, var_lon, lon)

nc_close(nc_out)

# ----------------------------
# 4️⃣
# ----------------------------
# Convert to point dataframe
# Flatten arrays pairwise
pts <- data.frame(
  lon = as.vector(lon),
  lat = as.vector(lat),
  TMP_max = as.vector(z_max)
)
pts$TMP_max[!is.finite(pts$TMP_max)] <- NA

# Convert points to SpatVector
pts_vect <- vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326")

# Create target raster
target_grid <- rast(
  xmin = -124.75, xmax = -103.5,
  ymin = 39, ymax = 53.5,
  resolution = 0.03,
  crs = "EPSG:4326"
)

# Rasterize with mean aggregation
r_interp <- rasterize(pts_vect, target_grid, field = "TMP_max", fun = mean)

plot(r_interp)

# Simple focal mean smoothing
r_smooth <- focal(r_interp, w = matrix(1,3,3), fun = mean, na.rm = TRUE)
plot(r_smooth)

#----------------------------
#----------Leaflet Mapping
#---------------------------

# Example: single layer from your SpatRaster
r <- r_smooth_stack[[55]]  # or any single-layer SpatRaster

# Ensure it has a CRS
crs(r) <- "EPSG:4326"

# Optional: define a color palette
pal <- colorNumeric(viridis(100), values(r), na.color = "transparent")

# Convert raster to leaflet-friendly tiles
r_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r), title = "TMP (K)")

r_map

# Save raster to GeoTIFF
out_file <- "data/temp/test/test.tif"
writeRaster(
  r_smooth,
  filename = out_file,
  overwrite = TRUE,
  filetype = "GTiff"
)
