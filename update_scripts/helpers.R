#-----------------------Folder Helper-----------------------
# ensures data folders exist first time app update is run
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) fs::dir_create(path)
}

#----------------------Cleanup Helper----------------------
clean_folder <- function(path) {
  files <- list.files(path, pattern = "\\.nc$", full.names = TRUE)
  
  keep <- grepl(update_date, files) | grepl(update_str, files) #removes files that don't have either current update_date (YYYY-MM-DD) or update_str (YYYYMMDD)
  remove <- files[!keep]
  
  if (length(remove)) file.remove(remove)
  invisible(length(remove))
}
