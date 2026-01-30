source("update_scripts/helpers.R")

stacks_to_smooth <- c(
  "GEOS_ocsmass_stack",
  "GEOS_VENT_RATE_stack"
)

for (stack_name in stacks_to_smooth) {
  
  if (!exists(stack_name, envir = .GlobalEnv)) {
    warning(glue::glue("Missing {stack_name}, skipping"))
    next
  }
  
  stack <- get(stack_name, envir = .GlobalEnv)
  
  # ----------------------------
  # Define finer grid
  # ----------------------------
  finer_res <- 0.027  # ~3 km
  
  r_fine <- rast(
    ext = ext(stack),
    resolution = finer_res,
    crs = crs(stack)
  )
  
  # ----------------------------
  # Resample (smooth)
  # ----------------------------
  stack_fine <- resample(stack, r_fine, method = "cubic")
  stack_fine <- clamp(stack_fine, lower = 0)
  # ----------------------------
  # Create new object name
  # ----------------------------
  fine_name <- sub("_stack$", "_fine_stack", stack_name)
  
  assign(fine_name, stack_fine, envir = .GlobalEnv)
  
  cat(glue::glue("Created smoothed stacks\n"))
}

#==================
#plot(stack_fine[[30]])


