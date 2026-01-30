library(shiny)
library(leaflet)
library(htmlwidgets)

get_latest_update_date <- function(dir_path = "data/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
  
  # Extract date part from filenames
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
  
  # Return the most recent date
  max(dates, na.rm = TRUE)
}

update_date <- get_latest_update_date()
# update_date <- "2024-07-25" #format(Sys.Date(), "%Y-%m-%d")

today <- update_date
airnow_today <- Sys.Date() #use Sys.Date() when operational or else AirNow hourly data won't plot properly between midnight and 8am (~model data update)

# Model Offset
montana_time <- as.POSIXlt(Sys.time(), tz = "America/Denver")
offset_hours <- montana_time$gmtoff / 3600

# utils
source(paste0("modules/utils/variable_utils.R"))
source(paste0("modules/utils/map_module_utils.R"))
source(paste0("modules/utils/plot_utils.R"))


# modules
source(paste0("modules/outlook_map_module.R"))
source(paste0("modules/hourly_map_module.R"))
source(paste0("modules/fire_table_module.R"))
source(paste0("modules/county_plot_module.R"))
source(paste0("modules/worm_plot_module.R"))
source(paste0("modules/model_performance_tile_module.R"))
source(paste0("modules/bias_plot_module.R"))
source(paste0("modules/model_performance_timeseries_module.R"))
source(paste0("modules/aqa_text_module.R"))
source(paste0("modules/aqa_map_module.R"))


ui <- fluidPage(
  fluidRow(
    column(4,
           selectInput("var_inp", "Variable:",
                       choices = c(
                         "Surface Smoke" = "MASSDEN",
                         "Temperature" = "TMP",
                         "Precipitation Rate" = "PRATE",
                         "Relative Humidity" = "RH",
                         "Wind Gust" = "GUST",
                         "Wind Speed" = "WIND_1hr_max_fcst",
                         "Boundary Layer Height" = "HPBL",
                         "Ventilation Rate" = "VENT_RATE"
                       ),
                       selected = "MASSDEN")
    ),
    column(4, sliderInput("layer", "Forecast Hour", min = 1, max = 48, value = 1, step = 1)),
    column(2, actionButton("play", "▶ Play / Pause")),
    column(2, sliderInput("speed", "Speed (ms)", min = 50, max = 500, value = 100, step = 50))
  ),
  leafletOutput("map", height = 600)
)

server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(montana_bounds[[1]][2], montana_bounds[[1]][1],
                montana_bounds[[2]][2], montana_bounds[[2]][1]) %>%
      addPolygons(
        data = mt_counties,
        fill = TRUE,
        fillOpacity = 0,
        color = "black",
        weight = .5,
        opacity = 1,
        popup = ~paste0("<strong>", NAME, " County</strong><br>")
      ) %>%
      add_airnow_layers(airnow_data) %>% 
      add_fire_layers(perim_data_sf, point_data_sf) %>%
      addLayersControl(
        overlayGroups = c(
          "Wildfires (perimeters)",
          "Wildfires (points)",
          "Monitors"
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>%
      onRender(sprintf("
        function(el, x) {
          var map = this;
          var bounds = [[%f, %f], [%f, %f]];
          window.imageLayers = [];
          for (var i=1; i<=48; i++) {
            var img = L.imageOverlay('MASSDEN/2025-08-05_' + String(i).padStart(2, '0') + '.png', bounds, {opacity: 0});
            img.addTo(map);
            window.imageLayers.push(img);
          }
          window.imageLayers[0].setOpacity(0.8);
        }
      ",
                       montana_bounds[[1]][1], montana_bounds[[1]][2],
                       montana_bounds[[2]][1], montana_bounds[[2]][2]
      ))
  })
  
  # JS handler for dynamic code
  insertUI(selector = "body", ui = tags$script(HTML("
    Shiny.addCustomMessageHandler('jsCode', function(message) {
      eval(message.code);
    });
  ")))
  
  # Handle variable changes -> reload overlays
  observeEvent(input$var_inp, {
    # JS to clear and reload images (as you already have)
    js_code <- sprintf("
    var map = HTMLWidgets.find('#map').getMap();
    if(window.imageLayers){
      window.imageLayers.forEach(function(layer){ map.removeLayer(layer); });
    }
    window.imageLayers = [];
    var bounds = [[%f, %f], [%f, %f]];
    for (var i=1; i<=48; i++) {
      var img = L.imageOverlay('%s/2025-08-05_' + String(i).padStart(2, '0') + '.png', bounds, {opacity: 0});
      img.addTo(map);
      window.imageLayers.push(img);
    }
    window.imageLayers[0].setOpacity(0.8);
  ",
                       montana_bounds[[1]][1], montana_bounds[[1]][2],
                       montana_bounds[[2]][1], montana_bounds[[2]][2],
                       input$var_inp)
    
    session$sendCustomMessage("jsCode", list(code = js_code))
    
    # --- Add Legend ---
    leafletProxy("map") %>%
      clearControls() %>%   # remove any old legends
      add_var_legend(input$var_inp)
  })
  
  # Play / pause cycling
  observeEvent(input$play, {
    playing(!playing())
  })
  
  observe({
    if (playing()) {
      invalidateLater(input$speed, session)
      isolate({
        new_layer <- ifelse(input$layer >= 48, 1, input$layer + 1)
        updateSliderInput(session, "layer", value = new_layer)
      })
    }
  })
  
  # Switch layers
  observeEvent(input$layer, {
    js_code <- sprintf("
      if(window.imageLayers){
        window.imageLayers.forEach(function(layer){layer.setOpacity(0);});
        window.imageLayers[%d - 1].setOpacity(0.8);
      }
    ", input$layer)
    
    session$sendCustomMessage("jsCode", list(code = js_code))
    
    forecast_datetime <- as.POSIXct(as.Date(update_date)) + lubridate::hours(input$layer) - lubridate::hours(offset_hours)
    
    leafletProxy("map", session) %>%
      clearControls() %>%
      addControl(
        html = paste0(
          "<div style='background: rgba(255,255,255,0.8);
             padding: 4px 8px; border-radius: 4px;
             font-size: 14px;'><strong>Forecast Date:</strong> ",
          format(forecast_datetime, "%Y-%m-%d %H:%M"), "</div>"
        ),
        position = "topright"
      ) %>%
      add_var_legend(input$var_inp)

    
  })
}

shinyApp(ui, server)
