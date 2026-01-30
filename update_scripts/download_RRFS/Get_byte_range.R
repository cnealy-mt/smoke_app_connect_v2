# Step 1: Get byte ranges
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

update_str  <- format(update_date, "%Y%m%d")
run_hour    <- "06"
forecast_hours <- sprintf("f%03d", 0:84)

vars <- tibble::tibble(
  var_level = c("8 m above ground", "10 m above ground", "10 m above ground", 
                "2 m above ground", "2 m above ground", "surface", "surface", "surface"),
  var_name = c("MASSDEN", "UGRD", "VGRD", "TMP", "RH", "GUST", "HPBL", "PRATE"),
  aerosol = c("Particulate organic matter dry", "", "", "", "", "", "", "")
)

# --- FUNCTION: parse the .idx file once ---
parse_idx_file <- function(idx_url) {
  message("Downloading idx file: ", idx_url)
  idx_lines <- tryCatch(readLines(idx_url), error = function(e) {
    warning("Failed to read idx file at ", idx_url)
    return(NULL)
  })
  if (is.null(idx_lines)) return(NULL)
  
  split_lines <- str_split_fixed(idx_lines, ":", 4)
  tibble(
    row_num = as.integer(split_lines[,1]),
    offset  = as.numeric(split_lines[,2]),
    desc    = split_lines[,4]
  )
}

# --- FUNCTION: get byte ranges for one variable from parsed idx ---
get_byte_range_from_idx <- function(idx_df, var_name, var_level, aerosol) {
  
  match_idx <- idx_df %>%
    filter(
      str_detect(desc, fixed(var_name)),
      str_detect(desc, fixed(var_level)),
      if (aerosol != "") str_detect(desc, fixed(aerosol)) else TRUE
    ) %>%
    arrange(row_num)
  
  # 🔴 DEBUG MISSING DATA
  if (nrow(match_idx) == 0) {
    message("No match for: ", var_name, " | ", var_level, " | ", aerosol)
    return(tibble())   # ← must be tibble(), not NULL
  }
  
  row_i <- which(idx_df$row_num == match_idx$row_num[1])
  start_byte <- match_idx$offset[1]
  end_byte <- if (row_i < nrow(idx_df)) idx_df$offset[row_i + 1] - 1 else NA
  
  tibble(
    var_name = var_name,
    var_level = var_level,
    aerosol = aerosol,
    start_byte = start_byte,
    end_byte = end_byte,
    desc = match_idx$desc[1]
  )
}


# --- MAIN LOOP: read idx once per forecast hour and get byte ranges for all variables ---
all_byte_ranges <- map_dfr(forecast_hours, function(fhr) {
  
  idx_url <- sprintf(
    "https://noaa-rrfs-pds.s3.amazonaws.com/rrfs_a/rrfs.%s/%s/rrfs.t%sz.prslev.3km.%s.na.grib2.idx",
    update_str, run_hour, run_hour, fhr
  )
  
  idx_df <- parse_idx_file(idx_url)
  if (is.null(idx_df)) return(tibble())  # skip if failed to download
  
  map_dfr(1:nrow(vars), function(i) {
    get_byte_range_from_idx(
      idx_df,
      vars$var_name[i],
      vars$var_level[i],
      vars$aerosol[i]
    ) %>%
      mutate(forecast_hour = fhr)
  })
})

print(all_byte_ranges)
