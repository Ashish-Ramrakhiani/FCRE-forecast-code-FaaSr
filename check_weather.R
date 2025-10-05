check_weather <- function(lake_directory, configure_run_file, config_set_name){

 # Log the starting directory
  initial_wd <- getwd()
  cat("\n=== DEBUG: initial_working_dir:", initial_wd, "===\n")
  
  # List all directories to see what we have
  all_dirs <- list.dirs(".", recursive = TRUE, full.names = TRUE)
  cat("=== DEBUG: total_dirs_found:", length(all_dirs), "===\n")
  
  # Search for configuration directory
  config_dirs <- all_dirs[grepl("configuration", all_dirs)]
  cat("=== DEBUG: config_dirs_found:", length(config_dirs), "===\n")
  
  if (length(config_dirs) > 0) {
    cat("=== DEBUG: first_config_dir:", config_dirs[1], "===\n")
    repo_root <- dirname(config_dirs[1])
    
    # Try to change directory and verify
    tryCatch({
      setwd(repo_root)
      new_wd <- getwd()
      cat("=== DEBUG: new_working_dir:", new_wd, "===\n")
    }, error = function(e) {
      cat("=== ERROR: setwd failed:", e$message, "===\n")
    })
  } else {
    # If no config dirs found, try looking in parent directories or /tmp/functions/
    cat("=== DEBUG: config_search - searching /tmp/functions ===\n")
    tmp_dirs <- list.dirs("/tmp/functions", recursive = TRUE, full.names = TRUE)
    config_dirs_tmp <- tmp_dirs[grepl("configuration", tmp_dirs)]
    
    if (length(config_dirs_tmp) > 0) {
      cat("=== DEBUG: found_in_tmp:", config_dirs_tmp[1], "===\n")
      repo_root <- dirname(config_dirs_tmp[1])
      setwd(repo_root)
      cat("=== DEBUG: new_working_dir:", getwd(), "===\n")
    } else {
      cat("=== ERROR: No configuration directory found anywhere! ===\n")
    }
  }
  
  # Final working directory check
  cat("=== DEBUG: final_working_dir before FLARE call:", getwd(), "===\n")
  
  # CRITICAL: Wrap the FLARE function call in error handling
  weather_ready <- tryCatch({
    result <- FLAREr::check_noaa_present(lake_directory,
                                          configure_run_file,
                                          config_set_name)
    cat("=== DEBUG: FLARE function completed successfully ===\n")
    result
  }, error = function(e) {
    cat("=== ERROR in FLARE function:", e$message, "===\n")
    cat("=== ERROR: Full error details ===\n")
    print(e)
    FALSE  # Return FALSE on error
  })
  
  cat("=== DEBUG: weather_ready =", weather_ready, "===\n")
  cat("=== DEBUG: Returning from check_weather ===\n")
  
  # ALWAYS return a simple value
  return(list(success = TRUE, weather_ready = weather_ready))

}
