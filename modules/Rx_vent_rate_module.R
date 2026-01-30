Rx_ventrate_ModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(
      ns("csv_in"),
      "Upload Rx Proposals",
      accept = ".csv"
    ),
    
    downloadButton(
      ns("csv_out"),
      "Download Rx Vent Rates"
    )
  )
}

Rx_ventrate_ModuleServer <- function(id, today) {
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded CSV
    input_df <- reactive({
      req(input$csv_in)
      readr::read_csv(input$csv_in$datapath, show_col_types = FALSE)
    })
    
    # Convert to terra vector
    points_vect <- reactive({
      df <- input_df()
      
      stopifnot(all(c("Latitude", "Longitude") %in% names(df)))
      
      terra::vect(
        df,
        geom = c("Longitude", "Latitude"),
        crs = "EPSG:4326",
        keepgeom = TRUE
      )
    })
    
    # Main extraction logic
    enriched_df <- reactive({
      pts <- points_vect()
      out_df <- input_df()
      
      for (lead in 1:9) {
        
        # Column name = date
        col_date <- today + lead
        col_name <- as.character(col_date)
        
        # Select raster path
        raster_path <- if (lead <= 2) {
          file.path(
            "data/VENT_RATE",
            sprintf(
              "%s_VENT_RATE_max_lead%d.tif",
              today,
              lead
            )
          )
        } else {
          file.path(
            "data/GEOS/VENT_RATE",
            sprintf(
              "%s_VENT_RATE_max_lead%d.tif",
              today,
              lead
            )
          )
        }
        
        if (!file.exists(raster_path)) {
          warning("Missing raster: ", raster_path)
          out_df[[col_name]] <- NA_real_
          next
        }
        
        r <- terra::rast(raster_path)
        
        # Reproject points if needed
        pts_proj <- terra::project(pts, terra::crs(r))
        
        # Extract raster values
        vals <- terra::extract(r, pts_proj)[[2]]
        
        # Append column (rounded to nearest whole number)
        out_df[[col_name]] <- round(vals)
      }
      
      out_df
    })
    
    # Download handler
    output$csv_out <- downloadHandler(
      filename = function() {
        paste0(today, "_Rx_proposals_vent.xlsx")
      },
      content = function(file) {
        
        df <- enriched_df()
        
        wb <- createWorkbook()
        addWorksheet(wb, "VentRate")
        
        writeData(wb, "VentRate", df)
        
        # Identify added date columns
        date_cols <- as.character(today + 1:9)
        col_idx <- match(date_cols, names(df))
        
        # Rows with data (skip header)
        data_rows <- 2:(nrow(df) + 1)
        
        for (col in col_idx) {
          
          conditionalFormatting(
            wb, "VentRate",
            cols = col,
            rows = data_rows,
            rule = "<235",
            style = createStyle(bgFill = "#FF0000")
          )
          
          conditionalFormatting(
            wb, "VentRate",
            cols = col,
            rows = data_rows,
            rule = ">=235",
            style = createStyle(bgFill = "#FF7E00")
          )
          
          conditionalFormatting(
            wb, "VentRate",
            cols = col,
            rows = data_rows,
            rule = ">=2350",
            style = createStyle(bgFill = "#FFFF00")
          )
          
          conditionalFormatting(
            wb, "VentRate",
            cols = col,
            rows = data_rows,
            rule = ">=4700",
            style = createStyle(bgFill = "#00E400")
          )
        }
        
        
        
        # Optional: auto column width
        setColWidths(wb, "VentRate", cols = 1:ncol(df), widths = "auto")
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
  })
}
