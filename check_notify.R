## USE THIS FUNCTION TO SEND NOTIFICATION (EMAIL/SLACK/ETC.) REGARDING THE HOX ACTIVITY
check_notify <- function(){


  ## send email or slack communication

  # lake_directory <- here::here()
  #
  # ## We want to update the HOx_off restart file
  #
  # config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file,
  #                                     lake_directory = lake_directory,
  #                                     config_set_name = config_set_name)

  # # check if Hox forecast exists
  # forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
  #
  # hox_on_df <- arrow::open_dataset(forecast_s3) |>
  #   dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
  #   dplyr::filter(model_id == 'glm_aed_flare_v3_faasr_HOx_ots', ## just using test model_id, need to change this
  #                 site_id == config$location$site_id,
  #                 variable == 'Temp_C_mean') |>
  #   dplyr::collect()
  #
  # if (nrow(hox_on_df) > 0){
  #   notify = TRUE
  #   ## send email or slack communication
  # } else{
  #   notify = FALSE
  # }

}
