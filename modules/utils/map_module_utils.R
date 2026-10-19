library(terra)
library(leaflet)
library(viridis)
library(lubridate)
library(tigris)
library(RColorBrewer)
library(sf)
library(scales)

# ===================================PNG Anchors=========================================
montana_bounds <- readRDS("modules/utils/png_bounds.rds")

#------------------------------------AQI Scale-------------------------------------------

# AQI
aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)
aqi_colors <- c(
  "#00E400",  # "Good"
  "#FFFF00",  # "Moderate"
  "#FF7E00",  # "USG"
  "#FF0000",  # "Unhealthy"
  "#8F3F97",  # "Very Unhealthy"
  "#7E0023"   # "Hazardous"
)


#------------------------------------AQI Trend Scale-------------------------------------------

# Define the discrete AQI trend steps
aqi_trend_values <- -5:5  # from -5 to +5

# Define the colors: green for negative, white at 0, red for positive
aqi_trend_colors <- c(
  colorRampPalette(c("#006400", "#ADFF2F"))(5),  # 5 greens (-5 to -1)
  "white",                                      # 0
  colorRampPalette(c("#FFB6C1", "#8B0000"))(5)   # 5 reds (+1 to +5)
)

# Create the palette that maps exact values
aqi_trend_palette <- colorFactor(
  palette = aqi_trend_colors,
  domain = aqi_trend_values,
  na.color = "transparent"
)

#------------------------------------Trend Scale-------------------------------------------
# the trend scale is custom fit to each variable and is therefore created in the trend mapping module

#------------------------------------Variable Scales-------------------------------------------

# MASSDEN
MASSDEN_breaks <- c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25, 30, 40, 60, 100, 200, Inf)
MASSDEN_colors <- c("transparent", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")
# Create color function
MASSDEN_palette <- function(x) {
  ifelse(x < 1, "#FFFFFF00", colorBin(
    palette = MASSDEN_colors[-1], 
    bins = MASSDEN_breaks[-1], 
    na.color = "transparent"
  )(x))
}

# PRATE
PRATE_breaks <- c(0, 0.00125, 0.00375, 0.0125, 0.0375, 0.0625, 0.09375, 0.125, 0.1875, 0.25, 0.375, 0.5, 0.625, 0.75, 1, Inf
)
PRATE_colors <- c("transparent", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")
# Create color function
PRATE_palette <- function(x) {
  ifelse(x < 0.00125, "#FFFFFF00", colorBin(
    palette = PRATE_colors[-1], 
    bins = PRATE_breaks[-1], 
    na.color = "transparent"
  )(x))
}

# GUST
GUST_palette <- colorBin(
  palette = rev(brewer.pal(11, "RdYlBu")),  # Reverse RdYlBu color scale
  domain = c(-Inf, Inf),                    # Full range (including outliers)
  bins = c(-Inf, seq(0, 20, length.out = 21), Inf),  # Fixed bin structure
  na.color = "transparent"
)

# HPBL
HPBL_palette <- colorBin(
  palette = rev(brewer.pal(11, "RdYlBu")),  # Reverse RdYlBu color scale
  domain = c(-Inf, Inf),                    # Full range (including outliers)
  bins = c(-Inf, seq(0, 4000, length.out = 21), Inf),  # Fixed bin structure
  na.color = "transparent"
)

# RH
RH_palette <- colorBin(
  palette = rev(brewer.pal(11, "RdYlBu")),  # Reverse RdYlBu color scale
  domain = c(-Inf, Inf),                    # Full range (including outliers)
  bins = c(-Inf, seq(0, 100, length.out = 21), Inf),  # Fixed bin structure
  na.color = "transparent"
)

# TMP
TMP_palette <- colorBin(
  palette = rev(brewer.pal(11, "RdYlBu")),  # Reverse RdYlBu color scale
  domain = c(-Inf, Inf),                    # Full range (including outliers)
  bins = c(-Inf, seq(0, 100, length.out = 21), Inf),  # Fixed bin structure
  na.color = "transparent"
)

# WIND_1hr_max_fcst
WIND_1hr_max_fcst_palette <- colorBin(
  palette = rev(brewer.pal(11, "RdYlBu")),  # Reverse RdYlBu color scale
  domain = c(-Inf, Inf),                    # Full range (including outliers)
  bins = c(-Inf, seq(0, 20, length.out = 21), Inf),  # Fixed bin structure
  na.color = "transparent"
)


# VENT_RATE_categorical
VENT_RATE_categorical_breaks <- c(0, 58.75, 117.5, 176.25, 235, 470, 940, 1410, 1880, 2350, 2820, 3290, 3760, 4230, 4700, Inf
)
VENT_RATE_categorical_colors <- c("#5F3A91","#8760AF","#B187CD","#DBAEEE","#7B1D11","#8D342D","#A75B57","#C38380","#DFADAB","#E59230","#E9A935","#EDC23F","#F3DA47","#F9F34F","#FFFFFF")
# Create color function
VENT_RATE_categorical_palette <- colorBin(palette = VENT_RATE_categorical_colors, bins = VENT_RATE_categorical_breaks, na.color = "transparent")

# VENT_RATE
VENT_RATE_breaks <- c(0, 235, 2350, 4700, Inf
)
VENT_RATE_colors <- c("#FF0000", "#FF7E00", "#FFFF00", "#00E400")  
# Create color function
VENT_RATE_palette <- colorBin(palette = VENT_RATE_colors, bins = VENT_RATE_breaks, na.color = "transparent")
VENT_RATE_labels <- c("Very Poor", "Poor", "Moderate", "Good")


# VENT_WINDOW
VENT_WINDOW_breaks <- c(0, 3, 6, 9, 24
)
VENT_WINDOW_colors <- c("#FF0000", "#FF7E00", "#FFFF00", "#00E400")
# Create color function
VENT_WINDOW_palette <- colorBin(palette = VENT_WINDOW_colors, bins = VENT_WINDOW_breaks, na.color = "transparent")


#------------------------------------Load State Boundary-------------------------------------------

# Download the state boundaries and filter for Montana (state FIPS = "30")
mt_state_boundary <- states(cb = TRUE, year = 2022) %>%
  dplyr::filter(STUSPS == "MT")

# Transform the CRS to match with your map (e.g., EPSG:4326)
mt_state_boundary <- st_transform(mt_state_boundary, crs = "EPSG:4326")

#------------------------------------Load County Boundaries-------------------------------------------

# Download all U.S. counties, then filter for Montana (state FIPS = "30")
mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")

#======================Monitor AQI Points (add to map function ======================================
airnow_data <- readRDS("data/AirNow/AirNow.rds")

add_airnow_layers <- function(map, airnow_df) {
  # Ensure latest reading per site
  latest_airnow <- airnow_df %>%
    group_by(site_name) %>%
    filter(local_time == max(local_time)) %>%
    ungroup()
  
  # AQI color palette
  aqi_palette <- colorBin(
    palette = aqi_colors,
    bins = aqi_breaks,
    na.color = "transparent",
    right = FALSE
  )
  
  # Popup content
  popup_content <- paste0(
    "<strong>", latest_airnow$site_name, "</strong><br>",
    "Value: ", round(latest_airnow$sample_measurement, 1), " ", latest_airnow$units_of_measure, "<br>",
    "Last Updated: ", latest_airnow$local_time, "<br>",
    "Agency: ", latest_airnow$monitoring_agency
  )
  
  map %>%
    addCircleMarkers(
      lng = latest_airnow$longitude,
      lat = latest_airnow$latitude,
      group = "Monitors",
      radius = 6,
      color = "black",
      weight = 1,
      fillColor = aqi_palette(latest_airnow$sample_measurement),
      fillOpacity = 0.9,
      popup = popup_content
    ) 
}


#------------------------------------Fire Data-------------------------------------------
point_data_sf <- readRDS("data/fire_locs/fire_point.rds")
perim_data_sf <- readRDS("data/fire_locs/fire_poly.rds")

add_fire_layers <- function(map, perim_data_sf, point_data_sf) {
  
  point_data_filtered <- point_data_sf %>% filter(IncidentTypeCategory == "WF")
  perim_data_filtered <- perim_data_sf %>% filter(IncidentTypeCategory == "WF")
  
  popup_content_point <- ~paste0(
    "<strong>", point_data_filtered$IncidentName, "</strong><br>",  # IncidentName in bold
    point_data_filtered$UniqueFireIdentifier, "<br>",
    "Start Date: ", point_data_filtered$FireDiscoveryDateTime, "<br>",
    "Size: ", point_data_filtered$IncidentSize, " acres", "<br>",
    "Contained: ", point_data_filtered$PercentContained, "%", "<br>",
    "Type: ", point_data_filtered$IncidentTypeCategory, "<br>",
    "Updated: ", point_data_filtered$PointModDateTime, "<br>"
  )
  
  popup_content_perim <- ~paste0(
    "<strong>", perim_data_filtered$IncidentName, "</strong><br>",  # IncidentName in bold
    perim_data_filtered$UniqueFireIdentifier, "<br>",
    "Start Date: ", perim_data_filtered$FireDiscoveryDateTime, "<br>",
    "Size: ", perim_data_filtered$IncidentSize, " acres", "<br>",
    "Contained: ", perim_data_filtered$PercentContained, "%", "<br>",
    "Type: ", perim_data_filtered$IncidentTypeCategory, "<br>",
    "Updated: ", perim_data_filtered$PerimeterModDateTime, "<br>"
  )
  
  map %>%
    addPolygons(
      data = perim_data_filtered,
      group = "Wildfires (perimeters)",
      fillColor = "red",
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5,
      popup = popup_content_perim
    ) %>%
    addCircleMarkers(
      data = point_data_filtered,
      group = "Wildfires (points)",
      radius = 3,
      color = NA,
      fillColor = "black",
      fillOpacity = 0.8,
      popup = popup_content_point
    )
}

#================================Hourly Map Legend Function=================================
add_var_legend <- function(map, var, layerId = "varLegend") {
  if (var == "MASSDEN") {
    legend_pal <- colorBin(
      palette = MASSDEN_colors[-1],
      bins = MASSDEN_breaks[-1],
      na.color = "transparent"
    )
    map <- addLegend(map, 
                     pal = legend_pal, 
                     values = MASSDEN_breaks[-1],
                     title = get_label(var), 
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "PRATE") {
    legend_pal <- colorBin(
      palette = PRATE_colors[-1], 
      bins = PRATE_breaks[-1], 
      na.color = "transparent"
    )
    map <- addLegend(map, 
                     pal = legend_pal, 
                     values = PRATE_breaks[-1], 
                     title = get_label(var), 
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "TMP") {
    map <- addLegend(map, 
                     pal = TMP_palette, 
                     values = seq(0, 100, length.out = 21), 
                     title = get_label(var), 
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "VENT_RATE") {
    legend_pal <- colorBin(
      palette = VENT_RATE_colors,
      bins = VENT_RATE_breaks,
      na.color = "transparent"
    )
    map <- addLegend(map, 
                     pal = legend_pal, 
                     values = VENT_RATE_breaks, 
                     labels = VENT_RATE_labels, 
                     title = get_label(var), 
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "WIND_1hr_max_fcst") {
    map <- addLegend(map,
                     pal = WIND_1hr_max_fcst_palette,
                     values = seq(0, 20, length.out = 21),
                     title = get_label(var),
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "RH") {
    map <- addLegend(map,
                     pal = RH_palette,
                     values = seq(0, 100, length.out = 21),
                     title = get_label(var),
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "HPBL") {
    map <- addLegend(map,
                     pal = HPBL_palette,
                     values = seq(0, 4000, length.out = 21),
                     title = get_label(var),
                     position = "bottomright",
                     layerId = layerId)
    
  } else if (var == "GUST") {
    map <- addLegend(map,
                     pal = GUST_palette,
                     values = seq(0, 20, length.out = 21),
                     title = get_label(var),
                     position = "bottomright",
                     layerId = layerId)
  }
  
  return(map)
}


#--------------------------------NO DATA MESSAGE---------------------------------------------

add_no_data_message <- function(proxy, message_text) {
  proxy %>%
    clearShapes() %>%
    clearImages() %>%
    clearControls() %>%
    addControl(html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px; 
                              border-radius: 4px; font-size: 16px; color: red;'>
                              <strong>", message_text, "</strong></div>"),
               position = "topleft")
}



