# set_env_vars.R for use in python later
# must be set before reticulate is loaded

# RRFS - feeds Get_byte_range.R, called by subset_rrfs.R
update_str  <- format(update_date, "%Y%m%d")
run_hour    <- "06"
forecast_hours <- sprintf("f%03d", 0:84)

# GEOS - feeds download_GEOS.R
update_str_GEOS  <- format(update_date, "%Y%m%d")
run_hour_GEOS    <- "00"
