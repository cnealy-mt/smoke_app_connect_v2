
# Model Performance Tile Plot Module UI
model_performance_tile_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("tile_chart"), height = 600)
}

# Module Server
model_performance_tile_ModuleServer <- function(id, year, month, variable) {
  moduleServer(id, function(input, output, session) {
    output$tile_chart <- renderHighchart({
      req(year(), month(), variable())
      print(variable())
      year <- as.numeric(year()) # ensure numeric for using sprintf (converting dates)
      month <- as.numeric(month())
      
      # Step 1: Date range
      date_range <- seq(
        as.Date(sprintf("%d-%02d-01", year, month)),
        as.Date(sprintf("%d-%02d-01", year, month)) %m+% months(1) - days(1),
        by = "day"
      )
      
      # Step 2: Load & filter
      daily_model_performance <- readRDS("data/model_performance/daily_model_performance.rds")
      data <- daily_model_performance %>%
        select(site_name, date, all_of(variable())) %>%
        filter(as.Date(date) %in% date_range)
      
      # Step 3: Site/date grid
      site_levels <- unique(data$site_name)
      complete_data <- expand.grid(
        site_name = site_levels,
        date = date_range
      )
      
      # Step 4: Join to full data
      data_full <- complete_data %>%
        left_join(data, by = c("site_name", "date")) %>%
        mutate(
          day = day(date),
          x = day,
          y = match(site_name, site_levels),
          value = .data[[variable()]]
        )
      
      # Step 5: Format data for Highcharts
      heatmap_data <- data_full %>%
        transmute(x = x - 1, y = y - 1, value = value) %>%
        list_parse()
      
      # Step 6: Color scale
      if (grepl("accuracy", variable(), ignore.case = TRUE)) {
        
        aqi_accuracy_values <- -5:5
        aqi_accuracy_colors <- c(
          colorRampPalette(c("#922b21", "#fcf3cf"))(5),
          "white",
          colorRampPalette(c("#d1f2eb", "#1f618d"))(5)
        )
        data_classes_list <- lapply(seq_along(aqi_accuracy_values), function(i) {
          val <- aqi_accuracy_values[i]
          list(
            from  = val,
            to    = val,
            color = aqi_accuracy_colors[i],
            name  = paste(val)
          )
        })
      } else {
        aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 1000)
        aqi_colors <- c("#00E400", "#FFFF00", "#FF7E00",
                        "#FF0000", "#8F3F97", "#7E0023")
        data_classes_list <- lapply(seq_len(length(aqi_breaks) - 1), function(i) {
          list(
            from  = aqi_breaks[i],
            to    = aqi_breaks[i + 1],
            color = aqi_colors[i],
            name  = paste0(aqi_breaks[i], "–", aqi_breaks[i + 1])
          )
        })
      }
      
      
      # Step 7: Chart title
      plot_title <- {
        var <- variable()
        
        # ---- Observations ----
        if (var == "airnow_obs") {
          "Monitor Obs Concentration"
          
        } else {
          
          # ---- model detection ----
          model <- if (startsWith(var, "GEOS_")) "GEOS" else "RRFS"
          
          # ---- accuracy vs concentration ----
          is_accuracy <- grepl("accuracy", var)
          
          # ---- extract lead time ----
          lead <- sub(".*lead([0-9]+).*", "\\1", var)
          
          if (is_accuracy) {
            paste0(
              "AQI Difference - ", model,
              " minus Monitor Obs (", lead, "-day lead)"
            )
          } else {
            paste0(
              model, " Concentration (",
              lead, "-day lead)"
            )
          }
        }
      }
      
      
      # Step 8: Plot heatmap
      highchart() %>%
        hc_chart(type = "heatmap") %>%
        hc_title(text = plot_title) %>%
        hc_xAxis(
          title = list(text = "Day of Month"),
          categories = as.character(unique(data_full$x)),
          tickInterval = 1
        ) %>%
        hc_yAxis(
          title = list(text = "Site"),
          categories = site_levels,
          reversed = TRUE
        ) %>%
        hc_add_series(data = heatmap_data, type = "heatmap") %>%
        hc_colorAxis(dataClasses = data_classes_list, nullColor = "#e0e0e0") %>%
        hc_tooltip(formatter = JS(sprintf(
          "function() {
             var year = %d;
             var month = %d - 1;
             var day = this.point.x + 1;
             var date = new Date(year, month, day);
             var dateString = date.toISOString().split('T')[0];
             return 'Site: <b>' + this.series.yAxis.categories[this.point.y] + '</b>' +
                    '<br>Date: <b>' + dateString + '</b>' +
                    '<br>Value: <b>' + this.point.value?.toFixed(1) + '</b>';
           }", year, month
        )))
    })
  })
}

