hourly_map_ModuleUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("map"), height = 700)
}

hourly_map_ModuleServer <- function(id, today, model_inp, offset_hours, var_inp, layer, speed, playing, opacity) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    today_str <- format(today, "%Y-%m-%d")
    
    # ==========================
    # 1️⃣ Image configuration
    # ==========================
    image_config <- reactive({
      req(model_inp(), var_inp())
      
      if (model_inp() == "RRFS") {
        list(
          start_layer  = 0,
          n_layers     = 85,
          step         = 1,
          base_dir     = paste0(var_inp()),
          index_offset = 1,
          init_hour    = 6
        )
      } else if (model_inp() == "GEOS" && var_inp() == "MASSDEN") {
        list(
          start_layer  = 0,
          n_layers     = 81,
          step         = 3,
          base_dir     = "GEOS/MASSDEN",
          index_offset = 1,
          init_hour    = 0
        )
      } else if (model_inp() == "GEOS" && var_inp() == "VENT_RATE") {
        list(
          start_layer  = 0,
          n_layers     = 240,
          step         = 1,
          base_dir     = "GEOS/VENT_RATE",
          index_offset = 0,
          init_hour    = 0
        )
      }
    })
    
    # ==========================
    # 2️⃣ Slider → image index
    # ==========================
    layer_index <- reactive({
      cfg <- image_config()
      req(cfg$n_layers, is.numeric(cfg$n_layers))
      
      idx <- floor((layer() - cfg$start_layer) / cfg$step)
      
      idx <- max(0, idx)
      idx <- min(idx, cfg$n_layers - 1)
      
      as.integer(idx)
    })
    
    # ==========================
    # 3️⃣ Forecast datetime
    # ==========================
    forecast_datetime <- reactive({
      cfg <- image_config()
      as.POSIXct(
        paste(today, sprintf("%02d:00:00", cfg$init_hour)),
        tz = "UTC"
      ) + lubridate::hours(layer()) + lubridate::hours(offset_hours)
    })
    
    # ==========================
    # 4️⃣ Render map + initial images
    # ==========================
    output$map <- renderLeaflet({
      cfg <- image_config()
      leaflet() %>%
        addTiles() %>%
        fitBounds(
          montana_bounds[[1]][2], montana_bounds[[1]][1],
          montana_bounds[[2]][2], montana_bounds[[2]][1]
        ) %>%
        addPolygons(
          data = mt_counties,
          fill = TRUE,
          fillOpacity = 0,
          color = "black",
          weight = 0.5,
          opacity = 1,
          popup = ~paste0("<strong>", NAME, " County</strong><br>")
        ) %>%
        add_airnow_layers(airnow_data) %>%
        add_fire_layers(perim_data_sf, point_data_sf) %>%
        addLayersControl(
          overlayGroups = c("Wildfires (perimeters)", "Wildfires (points)", "Monitors"),
          options = layersControlOptions(collapsed = FALSE),
          position = "bottomleft"
        ) %>%
        onRender(sprintf("
          function(el, x) {
            var map = this;
            window.map = map;
            window.imageLayers = [];
            var bounds = [[%f, %f], [%f, %f]];
            for (var i = 0; i < %d; i++) {
              var idx = i + %d;
              var file_path = '%s/%s_' + String(idx).padStart(2,'0') + '.png';
              console.log('DEBUG PNG PATH (JS):', file_path);
              var img = L.imageOverlay(file_path, bounds, {opacity: 0});
              img.addTo(map);
              img.bringToFront();
              window.imageLayers.push(img);
            }
            window.imageLayers[0].setOpacity(0.8);
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }
        ",
                         montana_bounds[[1]][1], montana_bounds[[1]][2],
                         montana_bounds[[2]][1], montana_bounds[[2]][2],
                         cfg$n_layers, cfg$index_offset,
                         cfg$base_dir, today_str,
                         ns("map_rendered")
        )) %>%
        setView(lng = -110.0, lat = 47.0, zoom = 7)
    })
    
    # --- Initial datetime & legend once after map load ---
    observeEvent(input$map_rendered, {
      req(layer())  # wait until layer() is available

      dt <- forecast_datetime()
      leafletProxy("map", session) %>%
        removeControl("forecastTime") %>%
        addControl(
          html = paste0(
            "<div style='background: rgba(255,255,255,0.8);
            padding: 4px 8px; border-radius: 4px;
            font-size: 14px;'><strong>Forecast Date:</strong> ",
            format(dt, "%Y-%m-%d %H:%M"), "</div>"
          ),
          position = "topright",
          layerId = "forecastTime"
        ) %>%
        removeControl("varLegend") %>%
        add_var_legend(var_inp())
    }, once = TRUE)
    
    insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = tags$script(HTML("
    Shiny.addCustomMessageHandler('jsCode', function(message) {
      eval(message.code);
    });
  ")))
    
    
    # ==========================
    # 6️⃣ Play / pause animation
    # ==========================
    observe({
      if (playing()) {
        invalidateLater(speed(), session)
        isolate({
          cfg <- image_config()
          new_layer <- ifelse(layer() >= cfg$start_layer + (cfg$n_layers - 1) * cfg$step,
                              cfg$start_layer, layer() + cfg$step)
          updateSliderInput(session$rootScope(), "layer", value = new_layer)
        })
      }
    })
    
    # ==========================
    # 5️⃣ Update visible layer (opacity) on slider change
    # ==========================
    observeEvent(list(layer_index(), opacity()), {
      req(input$map_rendered)
      req(opacity())
      
      idx <- layer_index()
      req(is.finite(idx), idx >= 0)
      
      #message("OPACITY OBSERVER FIRED — idx = ", idx) #debug messages
      
      js_code <- sprintf("
    console.log('=== OPACITY UPDATE ===');
    console.log('Requested index:', %d);
    console.log('Total layers:',
      window.imageLayers ? window.imageLayers.length : 'NO imageLayers'
    );

    if (!window.imageLayers) {
      console.warn('imageLayers is undefined');
    } else {
      window.imageLayers.forEach(function(l, i) {
        l.setOpacity(0);
      });

      if (window.imageLayers[%d]) {
        console.log('Setting opacity for layer', %d);
        window.imageLayers[%d].setOpacity(%f);
      } else {
        console.warn('Layer index out of range:', %d);
      }
    }
  ", idx, idx, idx, idx, as.numeric(opacity()), idx)
      
      session$sendCustomMessage("jsCode", list(code = js_code))
    })
    
    
    
    
    # ==========================
    # 7️⃣ Update forecast time display
    # ==========================
    observeEvent(list(layer(), var_inp(), model_inp()), {
      req(layer())  # wait until layer() is available
      req(var_inp(), model_inp())
      
      dt <- forecast_datetime()
      
      leafletProxy("map", session) %>%
        removeControl("forecastTime") %>%
        addControl(
          html = paste0(
            "<div style='background: rgba(255,255,255,0.8);
        padding: 4px 8px; border-radius: 4px;
        font-size: 14px;'><strong>Forecast Date:</strong> ",
            format(dt, "%Y-%m-%d %H:%M"), "</div>"
          ),
          position = "topright",
          layerId = "forecastTime"
        ) %>%
        removeControl("varLegend") %>%
        add_var_legend(var_inp())
    })
    
  })
}



