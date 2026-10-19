aqa_map_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reset_map"), "Reset to Model Default", icon = icon("rotate-left"), style = "margin-bottom: 10px;"),
    leafletOutput(ns("aqa_map"), height = "400px"),
    downloadButton(ns("download_map_plot"), "Download Map Image")
  )
}

aqa_map_ModuleServer <- function(id, today, exp_time, exp_date, aqi_outlook_choice, aqa_thresh, aqa_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper function: load model defaults
    get_model_defaults <- function() {
      file_path <- if (aqi_outlook_choice() == "Tomorrow AQI Outlook") {
        paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead1.rds")
      } else {
        paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead0.rds")
      }
      
      if (!file.exists(file_path)) return(NULL)
      
      readRDS(file_path) %>%
        mutate(AQA_Required = if_else(MASSDEN > aqa_thresh(), "Yes", "No"))
    }
    
    # Initialize/Reset aqa_state whenever date, outlook, or threshold changes
    observe({
      df_default <- get_model_defaults()
      aqa_state(df_default)
    })
    
    # Reset Button Click Handler
    observeEvent(input$reset_map, {
      df_default <- get_model_defaults()
      aqa_state(df_default)
    })
    
    # Leaflet Shape Click Handler (Toggle Yes <-> No)
    observeEvent(input$aqa_map_shape_click, {
      click <- input$aqa_map_shape_click
      req(click$id)
      
      current_df <- aqa_state()
      req(current_df)
      
      updated_df <- current_df %>%
        mutate(AQA_Required = if_else(
          county == click$id,
          if_else(AQA_Required == "Yes", "No", "Yes"),
          AQA_Required
        ))
      
      aqa_state(updated_df)
    })
    
    # Render base Leaflet map once
    output$aqa_map <- renderLeaflet({
      leaflet(mt_counties) %>%
        setView(lng = -110.0, lat = 47.0, zoom = 6) %>%
        addProviderTiles("CartoDB.Positron")
    })
    
    # Dynamic Leaflet Update using Proxy (Fast, no full redraw)
    observe({
      req(aqa_state())
      
      mt_map_data <- mt_counties %>%
        left_join(aqa_state(), by = c("NAME" = "county"))
      
      pal <- colorFactor(
        palette = c("#004A98", "#F54D28"),
        levels = c("No", "Yes")
      )
      
      leafletProxy("aqa_map", data = mt_map_data) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          layerId = ~NAME, # CRITICAL: sets click$id
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
    
    # Plot function for PNG download using interactive state
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
        req(aqa_state())
        
        map_plot <- make_aqa_map_plot(
          mt_counties,
          aqa_state(),
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
