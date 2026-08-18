
# Module UI
worm_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_grid"))
  )
}

# Server
worm_plot_ModuleServer <- function(id, airnow_today, AirNow_select) {
  moduleServer(id, function(input, output, session) {
   
    # switched from using 'today' (i.e., update_date, as defined in top level of app) to system date/time for operational use; 
    # ensures that hourly monitoring data is always up to date, but the forecasts are correctly labeled (i.e., displays yesterdays forecasts and labels if new model run hasn't been updated for the day) 
    data <- reactive({
      # Decide which file to use based on selection
      file_suffix <- if (AirNow_select() == "running_avg") {
        "AirNow_running_avg.rds"
      } else {
        "AirNow.rds"
      }
      
      file_path <- file.path("data/AirNow", file_suffix)
      
      # Validate existence
      if (!file.exists(file_path)) {
        stop("❌ AirNow file not found: ", file_path)
      }
      
      df <- readRDS(file_path) 
      
      if (AirNow_select() == "hourly") {
        df <- df %>% mutate(sample_value = trunc(sample_measurement * 10) / 10)
      } else {
        df <- df %>% mutate(sample_value = trunc(sample_measurement_24hr_avg * 10) / 10)
      }
      
      df <- df %>%
        mutate(local_time_naive = force_tz(local_time, "UTC"),  # force UTC because Highcharter will converto local time for display
               local_time_ms = as.numeric(local_time_naive) * 1000)
    })
    
    # Zones
    zones <- get_zones("MASSDEN")
    
    # Reactive chart list
    charts <- reactive({
      df <- data()
      site_list <- split(df, df$site_name)
      
      imap(site_list, function(df_site, site) {
        highchart() %>%
          hc_add_series(
            data = df_site,
            type = "area",
            hcaes(x = local_time_ms, y = sample_value),
            name = if (AirNow_select() == "hourly") "Hourly PM2.5" else "24-hr Avg PM2.5",
            zones = zones,
            lineWidth = 5,
            fillOpacity = 0.5
          ) %>%
          hc_chart(margin = c(30, 0, 30, 50)) %>%
          hc_title(
            text = site,
            style = list(fontSize = "10px", fontWeight = "bold")
          ) %>%
          hc_xAxis(
            title = list(text = NULL),
            type = "datetime",
            labels = list(format = "{value:%b %d %H:%M}")
          ) %>%
          hc_yAxis(
            title = list(text = "µg/m³"),
            labels = list(enabled = TRUE)
          ) %>%
          hc_tooltip(
            pointFormat = "<b>{point.y}</b> µg/m³"
          ) %>%
          hc_legend(enabled = FALSE)
      })
    })
    
    # UI layout
    output$chart_grid <- renderUI({
      req(charts())
      
      bslib::layout_columns(
        col_widths = 3,
        gap = "1rem",
        !!!lapply(seq_along(charts()), function(i) {
          highchartOutput(session$ns(paste0("chart_", i)), height = "200px")
        })
      )
    })
    
    # Output each chart
    observe({
      req(charts())
      lapply(seq_along(charts()), function(i) {
        output[[paste0("chart_", i)]] <- renderHighchart({
          charts()[[i]]
        })
      })
    })
  })
}
