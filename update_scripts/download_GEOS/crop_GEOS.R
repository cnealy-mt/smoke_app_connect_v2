#crop_GEOS.R

source("update_scripts/helpers.R")

# ----------------------------
# 1️⃣ Define variables and file paths
# ----------------------------
vars_to_load <- c("ocsmass", "pblh", "v10m", "u10m")
update_str <- update_str_GEOS  # e.g., "20260102"
data_dir <- "data/temp"
crop_ext <- ext(-124.75, -103.5, 39, 53.5)

# ----------------------------
# 2️⃣ Function to load, crop, and optionally scale
# ----------------------------
load_crop_geos <- function(var_name) {
  file_path <- file.path(data_dir, paste0(var_name, "_GEOS_", update_str, ".nc"))
  r <- rast(file_path, subds = var_name)
  r_crop <- crop(r, crop_ext)
  
  if (var_name == "ocsmass") {
    r_crop <- r_crop * 1e9
  }
  
  return(r_crop)
}

# ----------------------------
# 3️⃣ Load all variables into a named list
# ----------------------------
GEOS_stack <- lapply(vars_to_load, load_crop_geos)

# Create object names like GEOS_ocsmass_stack
names(GEOS_stack) <- paste0("GEOS_", vars_to_load, "_stack")

# ----------------------------
# 4️⃣ Export each variable as its own object in memory
# ----------------------------
list2env(GEOS_stack, envir = .GlobalEnv)

# ----------------------------
# 5️⃣ Test plots
# ----------------------------
plot(GEOS_ocsmass_stack[[4]])  # 4th timestep of ocsmass
plot(GEOS_pblh_stack[[1]])     # first timestep of PBLH
