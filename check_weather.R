check_weather <- function(lake_directory, configure_run_file, config_set_name){

  dirs <- list.dirs(".", recursive = TRUE, full.names = TRUE)
  config_dirs <- dirs[grepl("configuration", dirs)]
  if (length(config_dirs) > 0) {
    repo_root <- dirname(config_dirs[1])
    setwd(repo_root)
    faasr_log(paste0('{"flare_config":"', repo_root, '"}'))
  }

  weather_ready <- FLAREr::check_noaa_present(lake_directory,
                                         configure_run_file,
                                         config_set_name)
  return(TRUE)
}
