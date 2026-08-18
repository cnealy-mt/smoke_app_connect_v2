#calculate_vent_rate_GEOS.R

source("update_scripts/helpers.R")

#----------------------------------------------------------------------------
#--------------------------------- CALCULATE WIND ---------------------------
#----------------------------------------------------------------------------
if (!all(dim(GEOS_v10m_stack) == dim(GEOS_u10m_stack))) {
  stop("v10m and u10m stacks do not have matching dimensions!")
}

GEOS_WIND_stack <- sqrt(GEOS_v10m_stack^2 + GEOS_u10m_stack^2)

#plot(GEOS_WIND_stack[[1]], main = "Wind speed (m/s)")

# #check for missing values
# r <- rast("data/temp/u10m_GEOS_20260104.nc")
# unique(values(r[[1]]))
# names(r)
# summary(r[[100:105]])

#----------------------------------------------------------------------------
#--------------------------------- CALCULATE VENT_RATE ---------------------------
#----------------------------------------------------------------------------
if (!is.null(GEOS_WIND_stack) & !is.null(GEOS_pblh_stack)) {
  this_var_name <- "GEOS_VENT_RATE"
  
  GEOS_VENT_RATE_stack <- GEOS_WIND_stack * GEOS_pblh_stack
  
  cat(glue("Successfully processed GEOS VENT RATE for {update_date}\n"))
} else {
  cat(glue("VENT RATE processing skipped for {update_date} due to missing input files.\n"))
}

#plot(GEOS_VENT_RATE_stack[[5]], main = "Vent Rate")
