source("update_scripts/helpers.R")

# Ensure folders exist
ensure_dir("data/fire_locs")

# -------------------------------------- Get Point Fire Data ----------------------------

point_url <- "https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/WFIGS_Incident_Locations_Current/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

response <- GET(point_url)
if (http_status(response)$category != "Success") stop("Error fetching point data")

point_data <- fromJSON(content(response, "text"))$features %>%
  unnest(attributes) %>%
  unnest(geometry) %>%
  select(IncidentName, IncidentShortDescription, POOState, UniqueFireIdentifier, IncidentTypeCategory, FireDiscoveryDateTime, 
         PercentContained, IncidentSize, EstimatedCostToDate, FireBehaviorGeneral, FireBehaviorGeneral2, 
         FireBehaviorGeneral3, FireCause, FireCauseGeneral, FireCauseSpecific, TotalIncidentPersonnel, 
         ModifiedOnDateTime_dt, x, y) %>%
  rename(PointModDateTime = ModifiedOnDateTime_dt) %>%
  mutate(FireDiscoveryDateTime = format(as.POSIXct(as.numeric(FireDiscoveryDateTime)/1000, origin = "1970-01-01")),
         PointModDateTime = format(as.POSIXct(as.numeric(PointModDateTime)/1000, origin = "1970-01-01")))

point_data_sf <- st_as_sf(point_data, coords = c("x", "y"), crs = 4326)

# -------------------------------------- Get CAN Point Fire Data ----------------------------

# 1. Read CAN fire data, clean and rename columns
CAN_point_data <- read.csv("https://cwfis.cfs.nrcan.gc.ca/downloads/activefires/activefires.csv") %>%
  mutate(IncidentSize = hectares * 2.47105,  # hectares to acres approx.
         IncidentTypeCategory = "WF") %>%
  rename(IncidentName = firename, x = lon, y = lat, FireDiscoveryDateTime = startdate) %>%
  select(IncidentName, FireDiscoveryDateTime, IncidentTypeCategory, IncidentSize, x, y)

# 2. Read Canadian provinces shapefile and transform CRS to WGS84 (EPSG:4326)
province_shapefile <- "misc/shapefiles/CAN_provinces"
provinces <- st_read(province_shapefile) %>%
  st_transform(crs = 4326)

# 3. Convert CAN data to sf object (points)
CAN_point_data_sf <- st_as_sf(CAN_point_data, coords = c("x", "y"), crs = 4326)

# 4. Spatial join to attach province name from polygon
# IMPORTANT: Disable s2 if you get geometry validation errors
sf::sf_use_s2(FALSE)  # disable s2 temporarily (optional, if errors occur)
CAN_point_data_sf <- st_join(
  CAN_point_data_sf, 
  provinces %>% select(PRENAME), 
  left = TRUE
) %>%
  rename(POOState = PRENAME)
sf::sf_use_s2(TRUE)  # re-enable s2

province_map <- c(
  "Newfoundland and Labrador" = "CA-NL",
  "Prince Edward Island" = "CA-PE",
  "Nova Scotia" = "CA-NS",
  "New Brunswick" = "CA-NB",
  "Quebec" = "CA-QC",
  "Ontario" = "CA-ON",
  "Manitoba" = "CA-MB",
  "Saskatchewan" = "CA-SK",
  "Alberta" = "CA-AB",
  "British Columbia" = "CA-BC",
  "Yukon" = "CA-YT",
  "Northwest Territories" = "CA-NT",
  "Nunavut" = "CA-NU"
)

CAN_point_data_sf <- CAN_point_data_sf %>%
  mutate(POOState = recode(POOState, !!!province_map))

# 5. Combine with other point data (assuming point_data_sf exists)
point_data_sf <- bind_rows(point_data_sf, CAN_point_data_sf)

# -------------------------------------- Get Perimeter Fire Data ----------------------------

perim_url <- "https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

response <- GET(perim_url)
if (http_status(response)$category != "Success") stop("Error fetching perimeter data")

perim_data <- fromJSON(content(response, "text"))$features %>%
  unnest(attributes) %>%
  unnest(geometry) %>%
  select(attr_IncidentName, attr_IncidentShortDescription, attr_POOState, attr_UniqueFireIdentifier, 
         attr_IncidentTypeCategory, attr_FireDiscoveryDateTime, attr_PercentContained, attr_IncidentSize, 
         attr_EstimatedCostToDate, attr_FireBehaviorGeneral, attr_FireBehaviorGeneral2, attr_FireBehaviorGeneral3, 
         attr_FireCause, attr_FireCauseGeneral, attr_FireCauseSpecific, attr_TotalIncidentPersonnel, 
         attr_ModifiedOnDateTime_dt, rings) %>%
  rename_with(~ sub("^attr_", "", .x)) %>%  # Remove 'attr_' prefix from selected columns
  rename(UniqueFireIdentifier = UniqueFireIdentifier,  # Already renamed in previous step
         PerimeterModDateTime = ModifiedOnDateTime_dt,
         Perimeters = rings) %>%
  mutate(Perimeters = lapply(Perimeters, function(ring) {
    if (is.array(ring) && length(dim(ring)) == 3 && dim(ring)[1] == 1) {
      return(matrix(ring[1,,], ncol = 2))  # Convert format for all rows
    }
    return(ring)  # Keep correctly formatted entries unchanged
  })) %>%
  mutate(FireDiscoveryDateTime = format(as.POSIXct(as.numeric(FireDiscoveryDateTime)/1000, origin = "1970-01-01")),
         PerimeterModDateTime = format(as.POSIXct(as.numeric(PerimeterModDateTime)/1000, origin = "1970-01-01")))

# -------------------------------------- Convert Perimeters to sf Polygons ----------------------------

convert_to_sf <- function(perimeters) {
  if (is.matrix(perimeters)) perimeters <- list(perimeters)  # Ensure list format
  
  polygons <- lapply(perimeters, function(ring) {
    if (!is.matrix(ring)) ring <- as.matrix(ring)  # Ensure matrix format
    
    # Close polygons if necessary
    if (!all(ring[1, ] == ring[nrow(ring), ])) {
      ring <- rbind(ring, ring[1, ])  # Close polygon by repeating first point
    }
    
    st_polygon(list(ring))  # Convert to polygon object
  })
  
  return(if (length(polygons) == 1) polygons[[1]] else st_multipolygon(polygons))
}

sf_polygons <- st_sfc(lapply(perim_data$Perimeters, convert_to_sf))
perim_data_sf <- st_sf(perim_data, geometry = sf_polygons)
st_crs(perim_data_sf) <- 4326  # Set CRS

saveRDS(point_data_sf, "data/fire_locs/fire_point.rds")
saveRDS(perim_data_sf, "data/fire_locs/fire_poly.rds")

# # -------------------------------------- Plot Leaflet Map ----------------------------
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = perim_data_sf,
#     fillColor = "red",  
#     color = "black",    
#     weight = 1,         
#     opacity = 1,
#     fillOpacity = 0.5,
#     popup = ~UniqueFireIdentifier
#   ) %>%
#   addCircleMarkers(
#     data = point_data_sf,
#     radius = 4,
#     color = NA,
#     fillColor = "black",
#     fillOpacity = 0.8,
#     popup = ~UniqueFireIdentifier
#   )
