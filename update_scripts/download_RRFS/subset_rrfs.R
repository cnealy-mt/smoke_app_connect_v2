# Before anything, must get byte ranges
source("update_scripts/download_RRFS/Get_byte_range.R")

ensure_dir <- ("data/temp")
clean_folder("data/temp")


#1) access and subset RRFS data from AWS S3 (runs subset_rrfs.py) 
library(reticulate)

# Ensure Python and packages are set up
required_packages <- c(
  "herbie-data",
  "xarray",
  "cfgrib",
  "fsspec",
  "aiohttp",
  "requests",
  "numpy",
  "pandas",
  "matplotlib",
  "requests",
  "netCDF4"
)
#py_install(required_packages)

cat("✅ Using Python at:", py_config()$python, "\n")

# Push url modifiers Python namespace
py$update_str <- update_str
py$run_hour   <- run_hour

# Pass byte ranges and metadata to Python
py$metadata_df <- all_byte_ranges

reticulate::py_run_file("update_scripts/download_RRFS/subset_rrfs.py")  # ✅ Just runs the script



