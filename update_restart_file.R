update_restart_file <- function(config_run_file, config_set_name){

  library(lubridate)

  lake_directory <- here::here()

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

 lake_directory <- getwd()

  ## We want to update the HOx_off restart file

  config <- FLAREr::set_up_simulation(configure_run_file = config_run_file,
                                      lake_directory = lake_directory,
                                      config_set_name = config_set_name)

  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")

  FLAREr::update_run_config(lake_directory = lake_directory,
                            configure_run_file = config_run_file,
                            restart_file = restart_file,
                            start_datetime = start_datetime,
                            end_datetime = NA,
                            forecast_start_datetime = forecast_start_datetime,
                            forecast_horizon = config$run_config$forecast_horizon,
                            sim_name = config$run_config$sim_name,
                            site_id = config$location$site_id,
                            configure_flare = config$run_config$configure_flare,
                            configure_obs = config$run_config$configure_obs,
                            use_s3 = config$run_config$use_s3,
                            bucket = config$s3$restart$bucket,
                            endpoint = config$s3$restart$endpoint,
                            config = config,
                            use_https = TRUE)
}
