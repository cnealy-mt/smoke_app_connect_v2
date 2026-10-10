model_performance_timeseries_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("model_performance_timeseries"), height = 400)
}

model_performance_timeseries_ModuleServer <- function(id, site_name_mp, start_date_mp, end_date_mp, model_inp, lead_time) {
  moduleServer(id, function(input, output, session) {
    output$model_performance_timeseries <- renderHighchart({
      
      # Filter data
      model_performance_timeseries <- hourly_model_performance %>%
        filter(
          site_name == site_name_mp() &
            date >= start_date_mp() & date <= end_date_mp()
        )
      
      # Determine model prefix and friendly model name
      model_prefix <- if (model_inp() == "RRFS") "model_smoke" else "GEOS_model_smoke"
      model_name <- model_inp()  # e.g., "RRFS" or "GEOS" for chart titles/series
      
      # Dynamically create column names
      col_hourly <- paste0(model_prefix, "_lead", lead_time())
      col_24hr <- paste0("24hr_avg_", model_prefix, "_lead", lead_time())
      
      # Use tidy evaluation to select columns dynamically
      df <- model_performance_timeseries %>%
        mutate(
          local_time_utc = lubridate::force_tz(local_time, tzone = "UTC"),
          model_point_hourly = !!rlang::sym(col_hourly),
          model_point_24hr_running = !!rlang::sym(col_24hr),
          AirNow_hourly = airnow_obs,
          AirNow_24hr_running = `24hr_avg_airnow_obs`
        )
      
      # Helper for series names
      series_name_hourly <- paste(model_name, "Hourly")
      series_name_24hr <- paste(model_name, "24-hr Running Avg")
      
      # Create the highchart
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = paste(model_name, "vs. Observed Concentrations Over Time")) %>%
        hc_xAxis(
          type = "datetime",
          title = list(text = "Time")
        ) %>%
        hc_yAxis(
          title = list(text = "PM2.5 Concentration (µg/m³)")
        ) %>%
        hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          formatter = JS(
            "function () {
       let s = '<b>' + Highcharts.dateFormat('%Y-%m-%d %H:%M', this.x) + '</b>';
       this.points.forEach(function(point) {
         s += '<br/>' + point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 1) + '</b>';
       });
       return s;
     }"
          )
        ) %>%
        hc_add_series(
          name = series_name_hourly,
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = model_point_hourly
            ) %>%
            list_parse2(),
          color = "#F54D28"
        ) %>%
        hc_add_series(
          name = "AirNow Hourly",
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = AirNow_hourly
            ) %>%
            list_parse2(),
          color = "#004A98"
        ) %>%
        hc_add_series(
          name = series_name_24hr,
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = model_point_24hr_running
            ) %>%
            list_parse2(),
          color = "#F54D28",
          lineWidth = 5,
          opacity = .5,
          marker = list(enabled = FALSE)
        ) %>%
        hc_add_series(
          name = "AirNow 24-hr Running Avg",
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = AirNow_24hr_running
            ) %>%
            list_parse2(),
          color = "#004A98",
          lineWidth = 5,
          opacity = .5,
          marker = list(enabled = FALSE)
        )
      
    })
  })
}




  
