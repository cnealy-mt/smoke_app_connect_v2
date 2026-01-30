# download_GEOS.R

source("update_scripts/helpers.R")

#---------------------------------------------------
#----------DOWNLOAD GEOS-FP DATA--------------------
#---------------------------------------------------
library(reticulate)
cat("✅ Using Python at:", py_config()$python, "\n")

update_str_GEOS  <- format(update_date, "%Y%m%d")
run_hour_GEOS    <- "00"

py$update_str_GEOS <- update_str_GEOS
py$run_hour_GEOS   <- run_hour_GEOS

# continues with other processing if server is inaccessible
result <- try(
  reticulate::py_run_file("update_scripts/download_GEOS/download_GEOS.py"),
  silent = TRUE
)

if (inherits(result, "try-error")) {
  message("❌ GEOS download failed.")
  GEOS_OK <- FALSE
} else {
  message("✅ GEOS download succeeded.")
  GEOS_OK <- TRUE
}

# <-- THIS is key
GEOS_OK





