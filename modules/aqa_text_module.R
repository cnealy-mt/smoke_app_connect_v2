aqa_text_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("aqa_message")),
    downloadButton(ns("download_aqa_txt"), "Download AQA Text Product")
  )
}


aqa_text_ModuleServer <- function(id, today, airnow_today, aqi_outlook_choice, exp_time, exp_date, reason, aqa_thresh) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$aqa_message <- renderText({
      ""
    })
    
    output$download_aqa_txt <- downloadHandler(
      filename = function() {
        cat("Generating filename with today =", today, "\n")
        paste0(today, "_AQA_text_product.txt")
      },
      content = function(file) {

        # Determine the correct file path based on the outlook choice
        file_path <- if (aqi_outlook_choice() == "Tomorrow AQI Outlook") {
          paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead1.rds")
        } else {
          paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead0.rds")
        }
        
        if (!file.exists(file_path)) {
          writeLines("County data file not found.", file)
          return()
        } 
        
        df <- readRDS(file_path)
        
        df <- df %>%
          mutate(AQA_Required = if_else(MASSDEN > aqa_thresh(), "Yes", "No"))
        
        
        # Identify counties where AQA is required
        aqa_counties <- df %>%
          filter(AQA_Required == "Yes") %>%
          pull(county)
        
        # If "Today AQI Outlook", evaluate running averages from AirNow_avg
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
            
            # Combine and deduplicate counties
            aqa_counties <- unique(c(aqa_counties, AirNow_avg_recent))
            
            # Update AQA_Required to "Yes" for counties in AirNow_avg_recent
            df <- df %>%
              mutate(AQA_Required = if_else(county %in% AirNow_avg_recent, "Yes", AQA_Required))
          } else {
            message("⚠️ AirNow averages not being used for AQA text: file not found for ", airnow_today) 
          }
        }
        
        # Prepare list of all counties with AQA flag
        county_lines <- c(paste0(df$county, ";", df$AQA_Required), "")
        
        # Standard header
        header_lines <- c(
          glue::glue("-Expiration Time: {exp_time()} on {format(exp_date(), '%m/%d/%Y')}"),
          "",
          "-Counties",
          ""
        )
        
        # Optional alert message if any 'Yes' present
        alert_text <- character()
        if (length(aqa_counties) > 0) {
          alert_text <- c(
            "",
            "-Text for alert:",
            glue::glue(
              "The Montana Department of Environmental Quality has issued an air quality alert for {paste(aqa_counties, collapse = ', ')} counties in effect until {exp_time()} on {format(exp_date(), '%m/%d/%Y')}"
            ),
            "An Air Quality Alert means that particulates have been trending upwards and that an exceedence of the 24 hour National Ambient Air Quality Standard (NAAQS) has occurred or may occur in the near future.",
            ""  # <-- line break after alert
          )
        } 
        
        # REASON FOR ALERT--------------------------------------------------------------------------
        reason_text_block <- character()
        
        if (!is.null(reason()) && length(reason()) > 0 && nchar(trimws(reason())) > 0) {
          reason_text_block <- c(
            reason(),
            ""
          )
        }

        # CURRENT AQI--------------------------------------------------------------------------------
        
        airnow_file_path <- paste0("data/AirNow/AirNow.rds")
        
        # Load file if it exists, otherwise create an empty AirNow dataframe
        AirNow <- if (file.exists(airnow_file_path)) {
          readRDS(airnow_file_path)
        } else {
          message("⚠️ AirNow file for today not found. Proceeding with empty AirNow data frame.")
          
          # Create empty tibble with correct column names and types
          tibble(
            site_name = character(),
            date_gmt = as.POSIXct(character()),
            sample_measurement = numeric()
          )
        }
        
        aqi_labels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
        aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 1000)
        
        # Step 1: Filter to the most recent GMT date
        most_recent_gmt <- max(AirNow$date_gmt, na.rm = TRUE)
        
        # Step 2: Convert to MDT (UTC-6 or -7 depending on daylight saving; we'll use America/Denver)
        # `America/Denver` handles daylight saving automatically
        AirNow_filtered <- AirNow %>%
          filter(date_gmt == most_recent_gmt) %>%
          mutate(date_mdt = with_tz(date_gmt, tzone = "America/Denver"))
        
        # Step 3: Assign AQI category based on PM2.5 value
        AirNow_aqi <- AirNow_filtered %>%
          mutate(
            AQI_category = cut(
              sample_measurement,
              breaks = aqi_breaks,
              labels = aqi_labels,
              include.lowest = TRUE,
              right = TRUE
            )
          ) %>%
          select(site_name, date_mdt, sample_measurement, AQI_category)
        
        ordered_levels <- rev(aqi_labels)
        
        # Filter out "Good" and make AQI_category a factor with reversed levels
        aqi_filtered <- AirNow_aqi %>%
          filter(AQI_category != "Good") %>%
          mutate(AQI_category = factor(AQI_category, levels = ordered_levels))
        
        # Time stamp from first row
        report_time <- format(AirNow_aqi$date_mdt[1], "%I%p on %m/%d/%Y") %>%
          sub("^0", "", .)  # remove leading zero manually
        # e.g., 5AM on 7/25/2024
        
        # Function to format lists nicely
        pretty_list <- function(x) {
          if (length(x) == 0) return("")
          if (length(x) == 1) return(x)
          if (length(x) == 2) return(paste(x, collapse = " and "))
          paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
        }
        
        
        # Build one paragraph per AQI category
        aqi_text_chunks <- aqi_filtered %>%
          arrange(AQI_category) %>%
          group_by(AQI_category) %>%
          summarise(
            sites = pretty_list(site_name),
            .groups = "drop"
          ) %>%
          mutate(
            paragraph = glue("As of {report_time}, particulate levels in {sites} are {AQI_category}.")
          ) %>%
          pull(paragraph)
        
        # Join with blank lines between each
        final_aqi_text <- paste(aqi_text_chunks, collapse = "\n\n")
        final_aqi_text <- paste0(final_aqi_text, "\n")
        cat(final_aqi_text)
        
        # HEALTH MESSAGES---------------------------------------------------------------------
        
        health_messages <- list(
          "Hazardous" = "When air quality is Hazardous... State and local health officials recommend that everyone should avoid any outdoor exertion; people with respiratory or heart disease, the elderly, and children should remain indoors.",
          "Very Unhealthy" = "When air quality is Very Unhealthy... State and local health officials recommend that people with respiratory or heart disease, the elderly, and children should avoid any outdoor activity; everyone else should avoid prolonged exertion.",
          "Unhealthy" = "When air quality is Unhealthy... State and local health officials recommend that people with respiratory or heart disease, the elderly, and children should avoid prolonged exertion; everyone else should limit prolonged exertion.",
          "Unhealthy for Sensitive Groups" = "When air quality is Unhealthy for Sensitive Groups... State and local health officials recommend that people with respiratory or heart disease, the elderly and children should limit prolonged exertion.",
          "Moderate" = "When air quality is Moderate... State and local health officials recommend that unusually sensitive people should consider reducing prolonged or heavy exertion."
        )
        
        # AQI levels in reverse severity order (most severe first)
        aqi_levels <- c("Hazardous", "Very Unhealthy", "Unhealthy", 
                        "Unhealthy for Sensitive Groups", "Moderate")
        
        # Get unique AQI categories in the filtered data (excluding Good)
        present_categories <- unique(aqi_filtered$AQI_category)
        present_categories <- intersect(aqi_levels, present_categories)
        
        # Assemble messages
        health_text <- unlist(lapply(present_categories, function(cat) {
          c(health_messages[[cat]], "")  # Add empty line after each
        }))
        
        # MORE INFORMATION---------------------------------------------------------------------
        
        more_information <- paste0("For more information visit the Montana Department of Environmental Quality at http://todaysair.mtdeq.us")
        
        # Final output text
        full_text <- c(header_lines, county_lines, alert_text, reason_text_block, final_aqi_text, health_text, more_information)
        cat(full_text)
        
        # Write to file
        writeLines(full_text, file)
      }
    )
  })
}

