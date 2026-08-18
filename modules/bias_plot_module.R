# Bias Plot Module UI
bias_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("bias_plot"), height = 400)
}



# Module Server
bias_plot_ModuleServer <- function(id, year, month, lead_time, model_inp) {
  moduleServer(id, function(input, output, session) {
    output$bias_plot <- renderHighchart({
      
      req(lead_time())
      
      daily_model_performance <- readRDS("data/model_performance/daily_model_performance.rds") %>%
        filter(lubridate::month(date) %in% 5:10 &
                 lubridate::year(date) == year())
      
      # Determine model prefix and friendly model name
      model_prefix <- if (model_inp() == "RRFS") "model_smoke" else "GEOS_model_smoke"
      model_name <- model_inp()  # e.g., "RRFS" or "GEOS" for chart titles/series
      
      daily_model_performance <- daily_model_performance %>%
        mutate(
          model_value = !!rlang::sym(paste0(model_prefix, "_lead", lead_time())),
          accuracy_value = !!rlang::sym(paste0(model_prefix, "_lead", lead_time(), "_accuracy"))
        )
      
      

      bias <- daily_model_performance %>%
        select(model_value, airnow_obs, AQI_obs, accuracy_value) %>%
        drop_na() %>%
        mutate(bias = model_value - airnow_obs) %>%
        group_by(AQI_obs) %>%
        summarise(
          mean_bias = mean(bias),
          sample_count = n(),
          accuracy_pct = mean(accuracy_value %in% c(-1, 0, 1)) * 100  # Percent of predictions within ±1 category          .groups = "drop"
        )
      
      
      # 1. Map AQI numeric levels to labels and colors
      aqi_labels <- c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous")
      aqi_colors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
      
      # 2. Prepare chart data: filter to only the AQI levels you have data for
      chart_data <- bias %>%
        mutate(
          category = aqi_labels[AQI_obs],
          color = aqi_colors[AQI_obs]
        )
      
      # Helper for series names
      title_name <- paste(model_name, "Smoke Bias by AQI Category (µg/m³)")
      bias_name <- paste0("Mean Bias (", model_name, " - Obs)")
      
      # 3. Build the highchart
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = paste(title_name)) %>%
        hc_legend(enabled = FALSE) %>%  # <- Hide the legend and button
        hc_xAxis(
          categories = chart_data$category,
          title = list(text = "Observed AQI Category")
        ) %>%
        hc_yAxis(
          title = list(text = paste(bias_name)),
          plotLines = list(
            list(value = 0, color = "#000000", width = 2)  # Horizontal line at 0
          )
        ) %>%
        hc_add_series(
          name = "Mean Bias",
          data = pmap(
            list(
              y = chart_data$mean_bias,
              color = chart_data$color,
              sample_count = chart_data$sample_count,
              accuracy_pct = chart_data$accuracy_pct
            ),
            ~ list(
              y = ..1,
              color = ..2,
              sample_count = ..3,
              accuracy_pct = ..4
            )
          )
        ) %>%
        hc_tooltip(
          pointFormat = paste0(
            "Mean Bias: <b>{point.y:.2f}</b><br>",
            "Sample Count: <b>{point.sample_count}</b><br>",
            "Accuracy (Within 1 AQI Cat.): <b>{point.accuracy_pct:.1f}%</b>"
          )
        ) %>%
        hc_plotOptions(
          column = list(
            dataLabels = list(enabled = TRUE, format = "{point.y:.1f}"),
            pointPadding = 0,   # No space between the columns
            groupPadding = 0    # No space between the groups
          )
        )

    })
  })
}



