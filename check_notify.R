## USE THIS FUNCTION TO SEND NOTIFICATION (EMAIL/SLACK/ETC.) REGARDING THE HOX ACTIVITY
check_notify <- function(){


  ## send email or slack communication

  lake_directory <- here::here()

  configure_run_file <- "configure_run.yml"

  config_set_name <- 'glm_aed_flare_v3_faasr_HOx_off'
  #
  # ## We want to update the HOx_off restart file
  #
  config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file,
                                      lake_directory = lake_directory,
                                      config_set_name = config_set_name)

  server_name <- "forecasts_parquet"
  prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
  forecast_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix=prefix)


  # # check if Hox forecast exists
  # forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
  #
                                      
  hox_on_df <- arrow::open_dataset(forecast_s3) |>
  dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
  dplyr::filter(model_id == 'glm_aed_flare_v3_faasr_HOx_ots', ## just using test model_id, need to change this
                   site_id == config$location$site_id,
                   variable == 'Temp_C_mean') |>
     dplyr::collect()
  #
  if (nrow(hox_on_df) > 0){
    notify = TRUE
  #   ## send email or slack communication
   } else{
     notify = FALSE
   }

  notification_datetime <- Sys.time()
  notification_timestamp <- format(notification_datetime, "%Y%m%d_%H%M%S")
  
  # Create notification content
  notification_content <- paste0(
    "FaaSr Check Notify Status\n",
    "========================\n",
    "Timestamp: ", notification_datetime, "\n",
    "Notify Status: ", notify, "\n",
    "HOx forecast rows found: ", nrow(hox_on_df), "\n",
    "Model ID checked: glm_aed_flare_v3_faasr_HOx_ots\n",
    "Site ID: ", config$location$site_id, "\n"
  )
  
  # Write to local file
  local_folder <- "."
  local_file <- paste0("notify_status_", notification_timestamp, ".txt")
  writeLines(notification_content, file.path(local_folder, local_file))
  
  # Upload to S3
  server_name_upload <- "My_S3_Bucket"
  remote_folder <- "notifications"
  remote_file <- paste0("notify_status_", notification_timestamp, ".txt")
  
  FaaSr::faasr_put_file(
    server_name = server_name_upload,
    local_folder = local_folder,
    local_file = local_file,
    remote_folder = remote_folder,
    remote_file = remote_file
  )
  
  # Clean up local file
  file.remove(file.path(local_folder, local_file))
  
  print(paste0("Notification status file uploaded: notify=", notify, ", file=", remote_file))
  
  return(notify)

}
