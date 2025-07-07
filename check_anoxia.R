check_anoxia <- function(configure_run_file = "configure_run.yml",
                         config_set_name = 'glm_aed_flare_v3_faasr_HOx_off',
                         percentage_threshold = 20){

  library(dplyr)

  lake_directory <- here::here()

  config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file, lake_directory = lake_directory, config_set_name = config_set_name)

  reference_datetime <- config$run_config$forecast_start_datetime

  #config_set_name <- config$run_config$sim_name

  ## read in forecast of interest from S3
  #forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket,
                                  #endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)

server_name <- "forecasts_parquet"
prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
forecast_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix=prefix)

  forecast_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == 'glm_aed_flare_v3_faasr_HOx_off',
                  site_id == config$location$site_id,
                  variable == 'DO_mgL_mean',
                  reference_date == lubridate::as_datetime(reference_datetime)) |>
    dplyr::collect() |>
    filter(depth == max(depth)) |>
    mutate(prediction = prediction/1000*(32))

  anoxia_indication_list <- c()

  for (i in unique(forecast_df$parameter)){
    forecast_em <- forecast_df |>
      filter(parameter == i)

    em_min_do_value <- min(forecast_em$prediction)

    if (em_min_do_value < 2){
      anoxia <- 1
    } else {
      anoxia <- 0
    }

    anoxia_indication_list <- c(anoxia_indication_list, anoxia)

  }

  total_anoxia_members <- length(anoxia_indication_list)
  positive_anoxia_members <- sum(anoxia_indication_list)

  anoxia_member_ratio <- positive_anoxia_members/total_anoxia_members * 100

  if(anoxia_member_ratio >= percentage_threshold){
    anoxia_indication <- TRUE
  } else{
    anoxia_indication <- FALSE
  }

  return(anoxia_indication)

}
