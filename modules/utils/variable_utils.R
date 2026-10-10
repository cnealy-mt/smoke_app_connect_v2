# Variable utilities

#var_inp <- c("MASSDEN", "WIND_1hr_max_fcst", "TMP", "RH", "GUST", "HPBL", "PRATE", "VENT_RATE")

# Units lookup
get_unit <- function(var_inp) {
  units <- c(
    AQI_avg = "cat.",
    AQI_max = "cat.",
    MASSDEN = "µg/m³",
    MASSDEN_avg = "µg/m³",
    MASSDEN_max = "µg/m³",
    WIND_1hr_max_fcst = "m/s",
    WIND_1hr_max_fcst_max = "m/s",
    TMP = "°F",
    TMP_max = "°F",
    RH = "%",
    GUST = "m/s",
    HPBL = "m",
    PRATE = "in/hr",
    PRATE_acc = "inches",
    VENT_RATE = "m²/s",
    VENT_RATE_max = "m²/s",
    VENT_RATE_max_cty = "m²/s",
    VENT_WINDOW = "hours"
  )
  
  units[[var_inp]] %||% ""
}

# Variable Name lookup
get_var_name <- function(var_inp) {
  names <- c(
    AQI_avg = "AQI",
    AQI_max = "AQI",
    MASSDEN = "Surface Smoke",
    MASSDEN_avg = "Surface Smoke",
    MASSDEN_max = "Surface Smoke",
    WIND_1hr_max_fcst = "Wind Speed",
    WIND_1hr_max_fcst_max = "Wind Speed",
    TMP = "Temperature",
    TMP_max = "Temperature",
    RH = "Relative Humidity",
    GUST = "Wind Gust",
    HPBL = "Boundary Layer Height",
    PRATE = "Precip Rate",
    PRATE_acc = "Precip Daily Total",
    VENT_RATE = "Vent Rate",
    VENT_RATE_max = "Vent Rate",
    VENT_RATE_max_cty = "Vent Rate",
    VENT_WINDOW = "Vent Window"
  )
  
  names[[var_inp]] %||% var_inp  # fallback to var_inp if no name found
}

# Combined label (e.g., "Temperature (°F)")
get_label <- function(var_inp) {
  paste0(get_var_name(var_inp), " (", get_unit(var_inp), ")")
}

# Safe operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

#-------------------------Hourly Fcst Labels------------
get_hourly_label <- function(today, fcst_hour) {
  # Convert today into a proper datetime, adding 6 hours
  today_dt <- as.POSIXct(today) + hours(6)
  
  # Add fcst_hour to the datetime
  result_dt <- today_dt + hours(fcst_hour)
  
  # Format the result nicely
  return(format(result_dt, "%Y-%m-%d %H:%M"))
}

# # Example usage:
# today <- format(Sys.Date(), "%Y-%m-%d")  # Get today's date as a string
# fcst_hour <- 3  # Example forecast hour
# 
# get_hourly_label(today, fcst_hour)


