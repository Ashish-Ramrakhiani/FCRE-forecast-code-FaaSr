check_weather <- function(lake_directory, configure_run_file, config_set_name){

 # Log the starting directory
  initial_wd <- getwd()
  faasr_log(paste0('{"initial_working_dir":"', initial_wd, '"}'))
  
  # List all directories to see what we have
  all_dirs <- list.dirs(".", recursive = TRUE, full.names = TRUE)
  faasr_log(paste0('{"total_dirs_found":', length(all_dirs), '}'))
  
  # Search for configuration directory
  config_dirs <- all_dirs[grepl("configuration", all_dirs)]
  faasr_log(paste0('{"config_dirs_found":', length(config_dirs), '}'))
  
  if (length(config_dirs) > 0) {
    faasr_log(paste0('{"first_config_dir":"', config_dirs[1], '"}'))
    repo_root <- dirname(config_dirs[1])
    
    # Try to change directory and verify
    tryCatch({
      setwd(repo_root)
      new_wd <- getwd()
      faasr_log(paste0('{"new_working_dir":"', new_wd, '"}'))
    }, error = function(e) {
      faasr_log(paste0('{"setwd_error":"', e$message, '"}'))
    })
  } else {
    # If no config dirs found, try looking in parent directories or /tmp/functions/
    faasr_log('{"config_search":"searching /tmp/functions"}')
    tmp_dirs <- list.dirs("/tmp/functions", recursive = TRUE, full.names = TRUE)
    config_dirs_tmp <- tmp_dirs[grepl("configuration", tmp_dirs)]
    
    if (length(config_dirs_tmp) > 0) {
      faasr_log(paste0('{"found_in_tmp":"', config_dirs_tmp[1], '"}'))
      repo_root <- dirname(config_dirs_tmp[1])
      setwd(repo_root)
      faasr_log(paste0('{"new_working_dir":"', getwd(), '"}'))
    }
  }

  weather_ready <- FLAREr::check_noaa_present(lake_directory,
                                         configure_run_file,
                                         config_set_name)
  return(TRUE)
}
