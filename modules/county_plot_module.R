
# Module UI
county_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      uiOutput(ns("chart_grid"))
  )
}


county_plot_ModuleServer <- function(id, county, today, model_inp) {
  moduleServer(id, function(input, output, session) {
    req(model_inp)
    observeEvent(model_inp(), {
      message("Model changed to: ", model_inp())
    })
    hourly_data <- reactive({
      req(model_inp(), today)
      
      file_path <- if (model_inp() == "GEOS") {
        paste0("data/GEOS/county_hrly_avg/", today, "_county_hrly_avg.rds")
      } else {
        paste0("data/county_hrly_avg/", today, "_county_hrly_avg.rds")
      }
      
      #handle when model data is delayed (i.e., not available for today yet)
      if (!file.exists(file_path)) { 
        return(NULL)
      }
      
      readRDS(file_path)
    })
    

    # Define the variables to plot (excluding non-numeric or non-variable columns)
    vars_to_plot <- reactive({
      req(hourly_data())
      setdiff(names(hourly_data()), c("county", "fcst_hour", "time_local", "AQI_category"))
    })
    
    data_filtered <- reactive({
      req(county())
      req(hourly_data())  # make sure hourly_data is ready
      hourly_data() %>%   # <- note the ()
        filter(county == county())
    })
    
    
    # Generate time series charts for each variable
    charts <- reactive({
      req(data_filtered())
      req(vars_to_plot())
      
      map(vars_to_plot(), function(var) {
        data_var <- data_filtered()
        req(nrow(data_var) > 0)
        
        data_var <- data_var %>%
          arrange(time_local) %>%          # sort by time
          filter(!is.na(.data[[var]])) %>%
          mutate(
            local_time_naive = force_tz(time_local, "UTC"),
            local_time_ms = as.numeric(local_time_naive) * 1000
          )
        
        zones <- get_zones(var)
        
        highchart() %>%
          hc_add_series(
            data = data_var,
            type = "area",
            hcaes(x = local_time_ms, y = .data[[var]]),
            name = var,
            lineWidth = 2,
            zones = zones,
            fillOpacity = 0.4
          ) %>%
          hc_title(
            text = get_var_name(var),
            style = list(fontSize = "12px", fontWeight = "bold")
          ) %>%
          hc_chart(margin = c(30, 0, 30, 50)) %>%
          hc_xAxis(
            title = list(text = NULL),
            type = "datetime",
            labels = list(format = "{value:%b %d %H:%M}")
          ) %>%
          hc_yAxis(
            title = list(text = get_unit(var)),
            labels = list(format = "{value:.1f}")
          ) %>%
          hc_tooltip(
            valueDecimals = 1,
            valueSuffix = paste0(" ", get_unit(var))
          ) %>%
          hc_legend(enabled = FALSE)
      }) %>% setNames(vars_to_plot())
    })
    
    
    # Dynamically render chart grid UI
    output$chart_grid <- renderUI({
      if (is.null(hourly_data())) {
        div(
          class = "text-muted",
          style = "padding: 1rem;",
          icon("circle-exclamation"),
          strong("Model data not available yet."),
          tags$p(
            paste(
              "No forecast data was found for",
              today,
              "using the",
              model_inp(),
              "model."
            )
          )
        )
      } else {
        req(charts())
        
        bslib::layout_columns(
          col_widths = 4,  # Bootstrap 12 columns / 3 = 4
          gap = "1rem",
          !!!lapply(seq_along(charts()), function(i) {
            highchartOutput(session$ns(paste0("chart_", i)), height = "250px")
          })
        )
      }
    })
    
    # Render charts
    observeEvent(charts(), {
      chart_list <- charts()
      n <- length(chart_list)
      
      lapply(seq_len(n), function(i) {
        local({
          ii <- i
          output[[paste0("chart_", ii)]] <- renderHighchart({
            req(chart_list[[ii]])
            chart_list[[ii]]
          })
        })
      })
    })
    
    
  })
}

