# Timerseries utils
# Define your palettes and breaks in a list
palettes_list <- list(
  MASSDEN = list(
    breaks = c(0, 9, 35.4, 55.4, 125.4, 225.4, 1000),
    colors = c("#00E400","#FFFF00","#FF7E00","#FF0000", "#8F3F97","#7E0023")
    ),
  PRATE = list(
    breaks = c(0, 0.00125, 0.00375, 0.0125, 0.0375, 0.0625, 0.09375, 0.125, 0.1875, 0.25, 0.375, 0.5, 0.625, 0.75, 1, Inf),
    colors = c("white", "#D6EAF8", "#AED6F1", "#5DADE2", "#2874A6", "#117A65", "#27AE60", "#A0E424", "#FFF284", "#FFAD41", "#FF950A", "#FF6A00", "#C60000", "#970000", "#9A00FF")
  ),
  GUST = list(
    breaks = c(-Inf, seq(0, 20, length.out = 21), Inf),
    colors = rev(brewer.pal(11, "RdYlBu"))
  ),
  HPBL = list(
    breaks = c(-Inf, seq(0, 4000, length.out = 21), Inf),
    colors = rev(brewer.pal(11, "RdYlBu"))
  ),
  RH = list(
    breaks = c(-Inf, seq(0, 100, length.out = 21), Inf),
    colors = rev(brewer.pal(11, "RdYlBu"))
  ),
  TMP = list(
    breaks = c(-Inf, seq(0, 100, length.out = 21), Inf),  # 23 breaks → 22 bins
    colors = colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(22)  # Now 22 colors
  ),
  WIND_1hr_max_fcst = list(
    breaks = c(-Inf, seq(0, 20, length.out = 21), Inf),
    colors = rev(brewer.pal(11, "RdYlBu"))
  ),
  VENT_RATE = list(
    breaks = c(0, 235, 2350, 4700, Inf),
    colors = c("#FF0000", "#FF7E00", "#FFFF00", "#00E400")
  )
)

# Helper function to generate zones for highchart from a palette and breaks
get_zones <- function(var) {
  palette_info <- palettes_list[[var]]
  if (is.null(palette_info)) return(NULL)  # fallback if var is missing
  
  breaks <- palette_info$breaks
  colors <- palette_info$colors
  
  # Build zones list: all but last zone get a 'value', last is open-ended
  zones <- lapply(seq_along(colors), function(i) {
    if (i < length(colors)) {
      list(value = breaks[i + 1], color = colors[i])
    } else {
      list(color = colors[i])  # final zone has no 'value'
    }
  })
  
  return(zones)
}


montana_counties <- sort(c(
  "Beaverhead", "Big Horn", "Blaine", "Broadwater", "Carbon", "Carter", "Cascade", "Chouteau",
  "Custer", "Daniels", "Dawson", "Deer Lodge", "Fallon", "Fergus", "Flathead", "Gallatin",
  "Garfield", "Glacier", "Golden Valley", "Granite", "Hill", "Jefferson", "Judith Basin",
  "Lake", "Lewis and Clark", "Liberty", "Lincoln", "Madison", "McCone", "Meagher", "Mineral",
  "Missoula", "Musselshell", "Park", "Petroleum", "Phillips", "Pondera", "Powder River",
  "Powell", "Prairie", "Ravalli", "Richland", "Roosevelt", "Rosebud", "Sanders", "Sheridan",
  "Silver Bow", "Stillwater", "Sweet Grass", "Teton", "Toole", "Treasure", "Valley",
  "Wheatland", "Wibaux", "Yellowstone"
))

# Model Performance Timeseries 
hourly_model_performance <- readRDS("data/model_performance/hourly_model_performance.rds") # helps define calendar range for model performance tab 
  
# extract latest AirNow value
latest_non_na <- hourly_model_performance %>%
  filter(!is.na(airnow_obs)) %>%
  summarise(max_time = max(local_time)) %>%
  pull(max_time)

hourly_model_performance <- hourly_model_performance %>%
  filter(local_time <= latest_non_na) %>%
  mutate(date = as.Date(local_time, tz = "America/Denver"))


site_names <- hourly_model_performance %>%
  pull(site_name) %>%
  unique() %>%
  sort()
  










