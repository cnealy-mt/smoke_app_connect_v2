library(terra)
library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(highcharter)
library(purrr)
library(tidyr)
library(lubridate)
library(glue)
library(stringr)
library(writexl)
library(ggplot2)
library(htmlwidgets)
library(openxlsx)

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


# modules - RRFS
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
source(paste0("modules/Rx_vent_rate_module.R"))



ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      .header-bar {
      background-color: #004A98;
      color: white;
      padding: 0 20px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }

    .header-bar img {
      height: 45px;
      width: auto;
      margin-top: 10px;
      margin-bottom: 10px;
      display: block;
    }

    .header-bar h1 {
      margin: 0;
      font-size: 24px;
    }
    "))
  ),
  
  div(class = "header-bar",
      img(src = "logo_blue.png", alt = "Logo"),
      h1("AQB Smoke Forecasting Dashboard")
  ),
  #-----------------------------------------------------------------------------------------
  #----------------------------------------SIDEBAR-----------------------------------------------------
  #-----------------------------------------------------------------------------------------
  
  sidebar = sidebar(
    width = "20%",
    position = "right",
    open = "always",
    
    #------------------------------------Outlooks Tab-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks' || input.main_tabs == 'Hourly Forecast' || input.main_tabs == 'Model Performance'",
      selectInput(
        "model_inp",
        "Model:",
        choices = c("RRFS", "GEOS")
      )),
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks' && input.model_inp == 'RRFS'",
      selectInput("var_inp_outlook", "Variable:",
                  choices = c(
                    "AQI (county avg)" = "AQI_cty",
                    "AQI (avg)" = "AQI_avg",
                    "AQI (max)" = "AQI_max",
                    "PM2.5 (avg)" = "MASSDEN_avg",
                    "PM2.5 (max)" = "MASSDEN_max",
                    "Temp (max)" = "TMP_max",
                    "Precip (acc)" = "PRATE_acc",
                    "Wind (max)" = "WIND_1hr_max_fcst_max",
                    "Vent Rate (max, county)" = "VENT_RATE_max_cty",
                    "Vent Rate (max, gridded)" = "VENT_RATE_max",
                    "Ventilation Window" = "VENT_WINDOW"
                  )
      )),
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks' && input.model_inp == 'GEOS'",
      selectInput("var_inp_outlook", "Variable:",
                  choices = c(
                    "AQI (county avg)" = "AQI_cty",
                    "AQI (avg)" = "AQI_avg",
                    "AQI (max)" = "AQI_max",
                    "PM2.5 (avg)" = "MASSDEN_avg",
                    "PM2.5 (max)" = "MASSDEN_max",
                    "Vent Rate (max, county)" = "VENT_RATE_max_cty",
                    "Vent Rate (max, gridded)" = "VENT_RATE_max",
                    "Ventilation Window" = "VENT_WINDOW"
                  )
      )),
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks' && input.model_inp == 'RRFS'",
      selectInput("lead_time_outlook", "Lead Time (days):",
                  choices = c(0:2)
      )),
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks' && input.model_inp == 'GEOS'",
      selectInput("lead_time_outlook", "Lead Time (days):",
                  choices = c(0:9)
      )),
    
    conditionalPanel(
      condition = "(
          input.var_inp_outlook == 'VENT_WINDOW'
          ) && input.main_tabs == 'Outlooks'",
      selectInput("vent_category", "Ventilation Threshold:",
                  choices = c("Good or better" = "good", "Marginal or better" = "marginal"))
    )
    ,
    
    #------------------------------------Hourly Forecast Tab-------------------------------------------
    # One unconditional slider, speed, play button
    conditionalPanel(
      condition = "input.main_tabs == 'Hourly Forecast'",
      
      selectInput(
        "var_inp_hourly",
        "Variable:",
        choices = c()  # choices will be updated in server
      ),
      
      sliderInput(
        "layer",
        "Forecast Hour",
        min = 0,
        max = 84,
        value = 0,
        step = 1
      ),
      
      sliderInput(
        "speed",
        "Speed (ms)",
        min = 50,
        max = 500,
        value = 100,
        step = 50
      ),
      
      actionButton("play", "▶ Play / Pause")
    )
    ,
    
    
    #----------------------------------Other Tabs-----------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Worm Plots'",
      selectInput("AirNow_select", "Hourly or Running Avg:",
                  choices = c(
                    "Hourly" = "hourly",
                    "24hr Running Average" = "running_avg"
                  ))),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Model Performance'",
      selectInput(
        "lead_time",
        "Lead Time (days):",
        choices = NULL
      )
    ),
    
    
    
    
    #------------------------------------MISC Inputs-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs === 'Outlooks' || input.main_tabs === 'Hourly Forecast'",
      selectInput("transparency", "Opacity:",
                  choices = c("0%" = 0, "25%" = 0.25, "50%" = 0.5, "75%" = 0.75, "100%" = 1),
                  selected = 0.75),
      fire_table_ModuleUI("fire_table"), #fire table download button (has its own module file due to complexity)
      Rx_ventrate_ModuleUI("Rx_vent")
    ),
    
    #------------------------------------Text-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Outlooks'",
      p(""),
      h4("Outlooks"),
      p("Choose between the RRFS (~3 km grid, 84 fcst hours) and GEOS-FP (~25 km grid, 240 fcst hours) models above. Use the 'Variable' selector to switch between average (gridded or county area), maximum, and other statistics for the forecast day indicated by the 'Lead Time' selector (date displayed on map). Note: GEOS data are smoothed for display purposes.", style = "margin-left: 20px;"),
      p("Use the selector below the map to view area-averaged timeseries of select elements for each Montana county. ", style = "margin-left: 20px;"),
      
      h4("AQI Outlooks"),
      p("View average and maximum (1-hour) AQI values for the selected forecast day. Gridded and county area-averaged views are calculated from predicted 24hr-average near-surface (8 m AGL) PM2.5 concentrations on the selected forecast day.", style = "margin-left: 20px;"),
      img(src = "AQI_scale.png", width = "165px", alt = "AQI Color Scale", style = "margin-left: 20px;"),
      
      h4("Ventilation Rate"),
      p("Ventilation rate is calculated by multiplying wind (10 m AGL) by mixing height. Gridded and county area-averaged views are colored based on categorical breaks from the University of Washington.", style = "margin-left: 20px;"),
      p("Select the 'Ventilation Window' variable and a 'Ventilation Threshold' selector will appear. Choose either 'good' or 'marginal' to view the number of hours above the selected threshold.", style = "margin-left: 20px;"),
      
      h4("Fire Table"),
      p("Click on the 'Download' button above to save a table of current active wildfires. Fires are filtered to only include those in surrounding states/provinces greater than 5,000 acres or those in Montana greater than 100 acres. Fire data sourced from WFIGS (US) and CWFIS (Canada).", style = "margin-left: 20px;"),
      
      h4("Upload Burn Requests"),
      p("Upload a CSV table with 'Latitude' and 'Longitude' columns for requested burn locations (such as data downloaded from MIAG). Once uploaded, click 'Download Rx Vent Rates' to receive an Excel file with extracted vent rates at each fire's coordinates. Forecasted vent rates more than 2 days in the future are extracted from GEOS data.", style = "margin-left: 20px;"),
      
      p("All model data is scheduled to be updated by 6am MDT/7am MST. Periodically, GEOS data may be delayed a few hours.", style = "font-size: 90%; font-style: italic;"),
      
      p("This web app is experimental and intended for research purposes. Contact Cameron.Nealy@mt.gov with questions.", style = "font-size: 90%; font-style: italic;")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Hourly Forecast'",
      p(""),
      h4("Hourly Forecast"),
      p("Choose between the RRFS (~3 km grid, 84 fcst hours) and GEOS-FP (~25 km grid, 240 fcst hours) models above. Use the 'Variable' selector to switch between gridded elements. Click the 'play' button to animate the data across time. Note: GEOS data are smoothed for display purposes.", style = "margin-left: 20px;"),
      p("Use the selector below the map to view area-averaged timeseries of select elements for each Montana county. ", style = "margin-left: 20px;"),
      
      h4("Fire Table"),
      p("Click on the 'Download' button above to save a table of current active wildfires. Fires are filtered to only include those in surrounding states/provinces greater than 5,000 acres or those in Montana greater than 100 acres. Fire data sourced from WFIGS (US) and CWFIS (Canada).", style = "margin-left: 20px;"),
      
      h4("Upload Burn Requests"),
      p("Upload a CSV table with 'Latitude' and 'Longitude' columns for requested burn locations (such as data downloaded from MIAG). Once uploaded, click 'Download Rx Vent Rates' to receive an Excel file with extracted vent rates at each fire's coordinates. Forecasted vent rates more than 2 days in the future are extracted from GEOS data.", style = "margin-left: 20px;"),
      
      p("All model data is scheduled to be updated by 6am MDT/7am MST. Periodically, GEOS data may be delayed a few hours.", style = "font-size: 90%; font-style: italic;"),
      
      p("This web app is experimental and intended for research purposes. Contact Cameron.Nealy@mt.gov with questions.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Worm Plots'",
      p(""),
      h4("Hourly Monitor Data"),
      p("This panel streams PM2.5 AirNow data from all active Montana DEQ monitors. Hover over lines to view local time (MDT/MST) and values."),
      
      p("Data updated hourly.", style = "font-size: 90%; font-style: italic;"),
      
      p("This web app is experimental and intended for research purposes. Contact Cameron.Nealy@mt.gov with questions.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Model Performance'",
      p(""),
      h4("Tile Plot"),
      p("Compares monitor observed PM2.5 concentrations to model point forecast (at monitor location) in terms of AQI category. Accurate AQI category precition displayed as white square. Over prediction (tan/brown) and under prediction (blue) categorized by distance (i.e., number of AQI categories) of model prediction from observed category.", style = "margin-left: 20px;"),
      p("Change 'Variable' selector to view model or observed AQI categories with AQI color scale.", style = "margin-left: 20px;"),
      p("Note: 'RRFS' data prior to January 2026 was sourced from the 'HRRR' Model.", style = "margin-left: 20px;"),
      
      h4("Timeseries Plot"),
      p("Select an active or historical monitor to view observed and model PM2.5 concentrations over time.", style = "margin-left: 20px;"),
      
      h4("Bias Plot"),
      p("The bias plot displays model numerical mean bias binned by observed AQI categories at any monitor. Bias only calculated for 'wildfire season' (May through October).", style = "margin-left: 20px;"),
      
      p("Model performance data lags by one day.", style = "font-size: 90%; font-style: italic;"),
      
      p("This web app is experimental and intended for research purposes. Contact Cameron.Nealy@mt.gov with questions.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'AQA Text Product'",
      p(""),
      h4("Air Quality Alerts"),
      p("Create an AQA text file for coordination with the NWS by selecting the AQI outlook to use and clicking the download button. Expiration time defaults to 8AM. If the 'today' outlook is used, expiration date is set to tomorrow. If the 'tomorrow' outlook is used, expiration date is set to two days from now."),

      p("This tab only for use by Montana DEQ.", style = "font-size: 90%; font-style: italic;"),
      
      p("This web app is experimental and intended for research purposes. Contact Cameron.Nealy@mt.gov with questions.", style = "font-size: 90%; font-style: italic;")
      #p("Model performance data lags by one day.", style = "font-size: 90%; font-style: italic;")
      
    )
  ),
  #-----------------------------------------------------------------------------------------
  #---------------------------------------TABS---------------------------------------------
  #-----------------------------------------------------------------------------------------
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Outlooks",
             # Map module output
             outlook_map_ModuleUI("outlook_map"),
    ),
    tabPanel("Hourly Forecast",
             hourly_map_ModuleUI("hourly_map")
    ),
    tabPanel("Worm Plots",
             worm_plot_ModuleUI("worm_plot")
    ),
    tabPanel("Model Performance"),
    tabPanel("AQA Text Product") # moduleUI in conditionalPanel below
  ),
  #-----------------------------------------------------------------------------------------
  #-------------------------------------MAIN PLOT AREA--------------------------------------
  #-----------------------------------------------------------------------------------------
  
  #------------------------------------County Timeseries-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs === 'Outlooks' || input.main_tabs === 'Hourly Forecast'",       # Show plot only under these two tabs
    selectInput(
      inputId = "county",
      label = "Select County Forecast",
      choices = montana_counties, #from plot_utils.R
      selected = "Lewis and Clark"
    ),
    county_plot_ModuleUI("county_plot")
  ),
  
  #------------------------------------Model Performance-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs == 'Model Performance'",
    
    # Inputs row (3 inputs in one row)
    fluidRow(
      column(
        width = 4,
        selectInput("year_select", "Year:",
                    choices = seq(as.numeric(format(Sys.Date(), "%Y")) - 4,
                                  as.numeric(format(Sys.Date(), "%Y")))
                    ,
                    selected = as.numeric(format(Sys.Date(), "%Y")))
      ),
      column(
        width = 4,
        selectInput("month_select", "Month:",
                    choices = setNames(1:12, month.name),
                    selected = as.numeric(format(Sys.Date(), "%m")))
      ),
      column(
        width = 4,
        selectInput("tile_var", "Variable:",
                    choices = c(
                      "RRFS vs. Obs AQI" = "model_smoke_lead0_accuracy",
                      "RRFS Concentration" = "model_smoke_lead0",
                      "Obs Concentration" = "airnow_obs"
                    ))
      )
    ),
    
    # First plot (model performance tile)
    div(class = "py-3", model_performance_tile_ModuleUI("tile_plot")),
    
    # Second section: model performance time series and bias plot side by side
    fluidRow(
      # Left column: model performance time series and site/date inputs
      column(
        width = 6,
        
        # Inputs ABOVE the time series plot
        fluidRow(
          column(
            width = 6,
            selectInput("site_name_mp", "Monitoring Site:",
                        choices = site_names)
          ),
          column(
            width = 6,
            dateRangeInput(
              inputId = "date_range_mp",
              label = "Select Date Range:",
              start = max(hourly_model_performance$date)-days(1),
              end = max(hourly_model_performance$date),
              min = min(hourly_model_performance$date),
              max = max(hourly_model_performance$date),
              format = "yyyy-mm-dd",
              startview = "month"
            )
          )
        ),
        
        # Time series plot underneath
        model_performance_timeseries_ModuleUI("model_performance_timeseries")
      ),
      
      # Right column: bias plot + year selector above
      column(
        width = 6,
        
        # Year selector ABOVE the bias plot
        selectInput(
          inputId = "bias_plot_year",
          label = "Select Year:",
          choices = seq(as.numeric(format(Sys.Date(), "%Y")) - 4,
                        as.numeric(format(Sys.Date(), "%Y"))),
          selected = as.numeric(format(Sys.Date(), "%Y"))
        ),
        
        # Bias plot underneath
        bias_plot_ModuleUI("bias_plot")
      )
    )
  ),
  
  #------------------------------------AQA Tab-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs == 'AQA Text Product'",
    
    # Top section with inputs and map
    fluidRow(
      column(
        6,
        div(
          style = "margin-bottom: 10px;",
          radioButtons(
            inputId = "aqi_outlook_choice",
            label = "Use:",
            choices = c("Today AQI Outlook", "Tomorrow AQI Outlook"),
            selected = "Today AQI Outlook",
            inline = TRUE
          )
        ),
        div(
          style = "margin-bottom: 10px;",
          numericInput(
            inputId = "aqa_thresh",
            label = "AQA Threshold (µg/m³)",
            value = 35,
            min = 0,
            step = 0.1
          )
        ),
        div(
          style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
          textInput("exp_time", "Expiration Time", value = "8AM", width = "150px"),
          dateInput("exp_date", "Expiration Date", value = Sys.Date(), width = "200px")
        ),
        div(
          style = "margin-bottom: 10px;",
          textInput(
            "reason", 
            "Reason for alert:", 
            placeholder = "Optional: brief description of AQ & smoke conditions",
            width = "100%"
          )
        ),
        aqa_text_ModuleUI("aqa_message")
      ),
      
      column(
        6,
        aqa_map_ModuleUI("aqa_map")
      )
    )
  )
  
  
)




server <- function(input, output, session) {
  # outlook_map server-------------------------------------------------
  observeEvent(input$model_inp, {
    
    # Determine allowed lead times
    valid_leads <- if (input$model_inp == "RRFS") 0:2 else 0:9
    
    # Reset the lead_time input if current value is invalid
    if (!is.null(input$lead_time_outlook) && !(input$lead_time_outlook %in% valid_leads)) {
      updateSelectInput(
        session,
        "lead_time_outlook",
        choices = valid_leads,
        selected = min(valid_leads)  # or 0 as default
      )
    } else {
      # Also make sure choices are updated even if value is still valid
      updateSelectInput(
        session,
        "lead_time_outlook",
        choices = valid_leads
      )
    }
  })
  
  outlook_map_ModuleServer(
    id = "outlook_map",
    today = today,
    model_inp = reactive(input$model_inp),
    var_inp = reactive(input$var_inp_outlook),
    lead_time = reactive(input$lead_time_outlook),
    vent_category = reactive(input$vent_category),
    transparency = reactive(input$transparency),
    active_tab = reactive(input$main_tabs)
  )
  
  # hourly_map server-------------------------------------------------
  playing <- reactiveVal(FALSE)
  observeEvent(input$play, { playing(!playing()) })
  
  var_choices <- list(
    "RRFS" = c(
      "Surface Smoke" = "MASSDEN", 
      "Temperature" = "TMP", 
      "Precipitation Rate" = "PRATE", 
      "Relative Humidity" = "RH", 
      "Wind Gust" = "GUST", 
      "Wind Speed" = "WIND_1hr_max_fcst", 
      "Boundary Layer Height" = "HPBL", 
      "Ventilation Rate" = "VENT_RATE" 
    ),
    "GEOS" = c(
      "Surface Smoke" = "MASSDEN",
      "Ventilation Rate" = "VENT_RATE"
    )
  )
  
  # 1️⃣ Update variable choices when model changes
  observeEvent(input$model_inp, {
    req(input$model_inp)
    
    default_var <- "MASSDEN"
    
    updateSelectInput(
      session,
      "var_inp_hourly",
      choices = var_choices[[input$model_inp]],
      selected = default_var
    )
  })
  
  # 2️⃣ Update slider when variable changes
  observe({
    req(input$model_inp)
    req(input$var_inp_hourly)
    
    # determine slider limits
    new_min <- 0
    new_max <- if (input$model_inp == "RRFS") 84 else 240
    new_step <- if (input$model_inp == "RRFS") 1 else 3
    
    if (input$model_inp == "GEOS" && input$var_inp_hourly == "VENT_RATE") {
      new_min <- 1
      new_step <- 1
    }
    
    updateSliderInput(
      session,
      "layer",
      min = new_min,
      max = new_max,
      value = new_min,
      step = new_step
    )
  })
  
  
  hourly_map_ModuleServer(
    id = "hourly_map",
    today = today,
    offset_hours = offset_hours,
    model_inp = reactive(input$model_inp),
    var_inp = reactive(input$var_inp_hourly),
    layer = reactive(input$layer),
    speed = reactive(input$speed),
    playing = playing,
    opacity = reactive(input$transparency)
  )
  
  # fire_table server-------------------------------------------------
  fire_table_ModuleServer("fire_table")
  
  # Rx_vent_rate server------------------------------------------------
  Rx_ventrate_ModuleServer(
    id = "Rx_vent",
    today = today)
  
  # county_plot server-----------------------------------------------
  county_plot_ModuleServer(
    id = "county_plot",  # Same ID as in county_plot_UI
    today = today,
    county = reactive(input$county),  # From the global selectInput
    model_inp = reactive(input$model_inp)
  )
  
  # worm_plot server-------------------------------------------------
  worm_plot_ModuleServer(
    id = "worm_plot",
    airnow_today = airnow_today,
    AirNow_select = reactive(input$AirNow_select)
  )
  
  # model_performance_tile server-------------------------------------------------
  observeEvent(
    {input$main_tabs
      input$model_inp},
    
    {req(input$main_tabs == "Model Performance")
      req(input$model_inp)
      
      if (input$model_inp == "RRFS") {
        updateSelectInput(
          session,
          "lead_time",
          choices = 0:2,
          selected = 0
        )
      } else if (input$model_inp == "GEOS") {
        updateSelectInput(
          session,
          "lead_time",
          choices = 0:9,
          selected = 0
        )
      }
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    {
      input$model_inp
      input$lead_time
    },
    {
      req(input$model_inp, input$lead_time)
      
      model <- input$model_inp
      lead  <- as.integer(input$lead_time)
      
      max_lead <- if (model == "RRFS") 2 else 9
      req(lead <= max_lead)
      
      # ---- prefix logic ----
      model_prefix <- if (model == "GEOS") "GEOS_" else ""
      
      accuracy_var <- paste0(model_prefix, "model_smoke_lead", lead, "_accuracy")
      model_var    <- paste0(model_prefix, "model_smoke_lead", lead)
      
      obs_var <- if (lead == 0) {
        "airnow_obs"
      } else {
        "airnow_obs"
      }
      
      choices <- setNames(
        c(accuracy_var, model_var, obs_var),
        c(
          paste0(model, " vs. Obs AQI"),
          paste0(model, " Concentration"),
          "Obs Concentration"
        )
      )
      
      updateSelectInput(
        session,
        inputId = "tile_var",
        choices = choices,
        selected = accuracy_var
      )
    },
    ignoreInit = TRUE
  )
  
  
  
  
  
  model_performance_tile_ModuleServer(
    id = "tile_plot",
    year = reactive(input$year_select),
    month = reactive(input$month_select),
    variable = reactive(input$tile_var)
  )
  
  # bias_plot server-------------------------------------------------
  bias_plot_ModuleServer(
    id = "bias_plot",
    lead_time = reactive(input$lead_time),
    year = reactive(input$bias_plot_year),
    model_inp = reactive(input$model_inp)
  )
  
  # model_performance_timseries server-------------------------------------------------
  model_performance_timeseries_ModuleServer(
    id = "model_performance_timeseries",
    site_name_mp = reactive(input$site_name_mp),
    start_date_mp = reactive(input$date_range_mp[1]),
    end_date_mp = reactive(input$date_range_mp[2]),
    lead_time = reactive(input$lead_time),
    model_inp = reactive(input$model_inp)
  )
  
  # aqa_text server-------------------------------------------------
  observeEvent(input$aqi_outlook_choice, {
    new_date <- if (input$aqi_outlook_choice == "Today AQI Outlook") {
      Sys.Date() + 1
    } else {
      Sys.Date() + 2
    }
    
    updateDateInput(
      session,
      inputId = "exp_date",
      value = new_date
    )
  })
  
  aqa_text_ModuleServer(
    id = "aqa_message",
    today = today,
    airnow_today = airnow_today,
    aqi_outlook_choice = reactive(input$aqi_outlook_choice),
    exp_time = reactive(input$exp_time),
    exp_date = reactive(input$exp_date),
    reason = reactive(input$reason),
    aqa_thresh = reactive(input$aqa_thresh)
  )
  
  aqa_map_ModuleServer(
    id = "aqa_map",
    today = today,
    airnow_today = airnow_today,
    exp_time = reactive(input$exp_time),
    exp_date = reactive(input$exp_date),
    aqi_outlook_choice = reactive(input$aqi_outlook_choice),
    aqa_thresh = reactive(input$aqa_thresh)
  )
}


shinyApp(ui, server)
