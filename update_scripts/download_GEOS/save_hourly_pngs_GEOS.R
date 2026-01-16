source("update_scripts/helpers.R")

vars <- c("GEOS_ocsmass", "GEOS_VENT_RATE")

#switches GEOS naming to naming convention used throughout app logic
file_var_name <- function(var) {
  switch(
    var,
    GEOS_ocsmass   = "MASSDEN",
    GEOS_VENT_RATE = "VENT_RATE",
    var   # default: unchanged
  )
}

#-----------------------write PNGs-------------------
for (var in vars) {
  
  tryCatch({
    out_var <- file_var_name(var)
    
    this_var_name <- out_var
    stack_name <- paste0(var, "_fine_stack")
    
    # Pull object from memory
    stack <- get(stack_name, envir = .GlobalEnv)
    
    # Output folder
    folder_path <- file.path("www", "GEOS", out_var)
    if (!fs::dir_exists(folder_path)) {
      fs::dir_create(folder_path)
    }
    
    clean_folder(folder_path)
    
    source("update_scripts/write_hourly_png.R")
    
    cat(glue("Successfully processed {out_var} for {update_date}\n"))
    
  }, error = function(e) {
    cat(glue(
      "Warning: Failed to process {out_var} — {e$message}\n"
    ))
  })
}
