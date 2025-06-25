check_weather <- function(lake_directory, configure_run_file, config_set_name){

  weather_ready <- FLAREr::check_noaa_present(lake_directory,
                                         configure_run_file,
                                         config_set_name)
  return(weather_ready)
}
