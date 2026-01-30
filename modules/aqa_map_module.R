aqa_map_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("aqa_map"), height = "400px"),
    downloadButton(ns("download_map_plot"), "Download Map Image")
  )
}

aqa_map_ModuleServer <- function(id, today, airnow_today, exp_time, exp_date, aqi_outlook_choice, aqa_thresh) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ✅ Make df a reactive expression
    df <- reactive({
      # Main county-level file path
      file_path <- if (aqi_outlook_choice() == "Tomorrow AQI Outlook") {
        paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead1.rds")
      } else {
        paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead0.rds")
      }
      
      if (!file.exists(file_path)) return(NULL)
      
      df_local <- readRDS(file_path) %>%
        mutate(AQA_Required = if_else(MASSDEN > aqa_thresh(), "Yes", "No"))
      
      # ✅ Optionally augment with AirNow data if Today AQI Outlook is selected
      if (aqi_outlook_choice() == "Today AQI Outlook") {
        airnow_file_path <- paste0("data/AirNow/AirNow_running_avg.rds")
        
        if (file.exists(airnow_file_path)) {
          AirNow_avg <- readRDS(airnow_file_path)
          
          AirNow_avg_recent <- AirNow_avg %>%
            group_by(site_name) %>%
            filter(local_time == max(local_time, na.rm = TRUE)) %>%
            ungroup() %>%
            filter(sample_measurement_24hr_avg >= 35.4) %>%
            distinct(county) %>%
            pull(county)
          
          df_local <- df_local %>%
            mutate(AQA_Required = if_else(county %in% AirNow_avg_recent, "Yes", AQA_Required))
        } else {
          message("⚠️ AirNow averages not being used for AQA map: file not found for ", airnow_today)
        }
      }
      
      df_local
    })
    
    output$aqa_map <- renderLeaflet({
      req(df())  # Ensure df is available
      
      mt_map_data <- mt_counties %>%
        left_join(df(), by = c("NAME" = "county"))
      
      pal <- colorFactor(
        palette = c("#004A98", "#F54D28"),
        levels = c("No", "Yes")
      )
      
      leaflet(mt_map_data) %>%
        setView(lng = -110.0, lat = 47.0, zoom = 6) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(AQA_Required),
          fillOpacity = 1,
          color = "black",
          weight = 0.5,
          label = ~paste0(NAME, " County: ", AQA_Required),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~AQA_Required,
          title = "AQA",
          opacity = 1
        )
    })
    
    make_aqa_map_plot <- function(mt_counties, df_data, exp_time, exp_date) {
      counties_plot_data <- mt_counties %>%
        left_join(df_data, by = c("NAME" = "county")) %>%
        mutate(AQA_Required = factor(AQA_Required, levels = c("Yes", "No")))
      
      title_text <- glue::glue(
        "Air Quality Alert (expires {exp_time} on {format(exp_date, '%m/%d/%Y')})"
      )
      
      ggplot(counties_plot_data) +
        geom_sf(aes(fill = AQA_Required), color = "black", size = 0.2) +
        scale_fill_manual(values = c("Yes" = "#F54D28", "No" = "#004A98")) +
        theme_void() +
        labs(
          title = title_text,
          fill = "AQA"
        )
    }
    
    
    output$download_map_plot <- downloadHandler(
      filename = function() {
        paste0("aqa_map_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(df())
        
        map_plot <- make_aqa_map_plot(
          mt_counties,
          df(),
          exp_time = exp_time(),
          exp_date = exp_date()
        )
        
        ggsave(
          filename = file,
          plot = map_plot,
          device = "png",
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    )
    
  })
}

