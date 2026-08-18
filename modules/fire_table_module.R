

fire_table_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_fire_table"), "Download Fire Table")
  )
}

fire_table_ModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare the filtered fire data
    fire_point <- readRDS(paste0("data/fire_locs/fire_point.rds"))
    
    fire_point_filtered <- fire_point %>%
      filter(
        POOState %in% c("US-MT", "US-WA", "US-OR", "US-ID") & IncidentSize > 10) %>%
      filter(IncidentTypeCategory == "WF") %>%
      arrange(desc(as.Date(FireDiscoveryDateTime))) %>%
      mutate(State = sub("^(US-|CA-)", "", POOState)) %>%
      mutate(`Discovery Date` = {
        dt <- as.Date(FireDiscoveryDateTime)
        paste0(format(dt, "%B"), " ", as.integer(format(dt, "%d")), ", ", format(dt, "%Y"), sep = " ")
      }) %>%
      mutate(Updated = {
        dt <- as.Date(PointModDateTime)
        ifelse(
          is.na(dt),
          "Unknown",
          paste0(format(dt, "%B"), " ", as.integer(format(dt, "%d")), ", ", format(dt, "%Y"))
        )
      }) %>%
      mutate(
        Acres = formatC(round(IncidentSize), format = "d", big.mark = ",")
      ) %>%
      select(
        `Incident Name` = IncidentName,
        `State/Province` = State,
        `Discovery Date`,
        Acres,
        `% Contained` = PercentContained,
        Updated
      )
    
    fire_point_df <- fire_point_filtered %>% 
      sf::st_drop_geometry()
    
    output$download_fire_table <- downloadHandler(
      filename = function() {
        paste0("Fire_Incidents_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(
          list("Fires" = fire_point_df),
          path = file,
          format_headers = TRUE
        )
      }
    )
  })
}
