##  FUNCTION TO RUN FLARE FORECAST WITH NO ADDITIONAL OXYGENATION
## author: Austin Delany
## last modified: 2025-06-05

forecast_HOx_off <- function(configure_run_file = "configure_run.yml",
                             config_set_name = 'glm_aed_flare_v3_faasr_HOx_off'){
  # requires tidyverse and lubridate

  Sys.setenv('GLM_PATH'='GLM3r')
  options(future.globals.maxSize= 891289600)
  set.seed(100)

  lake_directory <- here::here()
  #config_set_name <- config$run_config$sim_name

  config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file,
                                      lake_directory = lake_directory,
                                      config_set_name = config_set_name,
                                      clean_start = TRUE)

  #Sys.setenv('AWS_ACCESS_KEY_ID' = Sys.getenv('AWS_ACCESS_KEY_ID_FAASR'))
  #Sys.setenv('AWS_SECRET_ACCESS_KEY' = Sys.getenv('AWS_SECRET_ACCESS_KEY_FAASR'))
  #Sys.setenv("AWS_DEFAULT_REGION" = "us-west-2")

  #Sys.setenv("AWS_DEFAULT_REGION" = config$s3$set_up$region,
             #"AWS_S3_ENDPOINT" = config$s3$set_up$endpoint,
             #"USE_HTTPS" = TRUE)

  noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                           configure_run_file,
                                           config_set_name = config_set_name)

  reference_date <- lubridate::as_date(config$run_config$forecast_start_datetime)

  #s3 <- arrow::s3_bucket(bucket = glue::glue(config$s3$vera_forecasts$bucket,"/project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=inflow_gefsClimAED"),
                         #endpoint_override = config$s3$vera_forecasts$endpoint,
                         #anonymous = TRUE)

    vera_base_path <- stringr::str_split_fixed(config$s3$vera_forecasts$bucket, "/", n = 2)[2]
    prefix <- glue::glue("{vera_base_path}/project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=inflow_gefsClimAED")
  
    server_name <- "vera_forecasts"
  
    #prefix <- "project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=inflow_gefsClimAED"
    s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                   faasr_prefix = prefix)
  
  avail_dates <- gsub("reference_date=", "", s3$ls())


  if(reference_date %in% lubridate::as_date(avail_dates)) {
    inflow_ready <- TRUE
  }else{
    inflow_ready <- FALSE
  }

  message(paste0("noaa ready: ", noaa_ready))
  message(paste0("inflow ready: ", inflow_ready))

  #while(noaa_ready & inflow_ready){

    #source(file.path(lake_directory, "workflows", config_set_name, "generate_inflow_forecast.R"))

  readr::read_csv(config$targets$vera_insitu_targets, show_col_types = FALSE) |>
    dplyr::mutate(observation = ifelse(variable == "DO_mgL_mean", observation*1000*(1/32), observation),
                  observation = ifelse(variable == "fDOM_QSU_mean", -151.3407 + observation*29.62654, observation),
                  depth_m = ifelse(depth_m == 0.1, 0.0, depth_m)) |>
    dplyr::rename(depth = depth_m) |>
    dplyr::filter(site_id == "fcre",
                  datetime >= lubridate::as_datetime(config$run_config$start_datetime)) |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
    readr::write_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")))

    ## I THINK THAT WE DONT NEED TO CALL INFLOW BEFORE RUNNING BECAUSE WE'RE USING INFLOW_S3 = TRUENo
  # s3_inflow <- arrow::open_dataset(arrow::s3_bucket(bucket = glue::glue(config$s3$inflow_drivers$bucket,'/',config$flows$future_inflow_model),
  #                                                   endpoint_override = config$s3$inflow_drivers$endpoint,
  #                                                   anonymous = TRUE)) |>
  #     dplyr::collect()

  FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  ## SCORE FORECASTS -- ADD SCORES TO FLARE bucket
  message("Scoring forecasts")
  
  source('./R/generate_forecast_score_arrow.R')

  message("sourced")

  # forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)

  server_name <- "forecasts_parquet"
  prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
  forecast_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix)

  
  message("call completed arrow")

  
  forecast_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == config$run_config$sim_name,
                  site_id == config$location$site_id,
                  reference_date == lubridate::as_datetime(config$run_config$forecast_start_datetime)) |>
    dplyr::collect()

  
  message("opened and collected")

  if(config$output_settings$evaluate_past & config$run_config$use_s3){

    message("reached inside if cond")
    #past_days <- lubridate::as_date(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon)
    past_days <- lubridate::as_date(lubridate::as_date(config$run_config$forecast_start_datetime) - lubridate::days(config$run_config$forecast_horizon))
   message("after past days")
    #vars <- arrow_env_vars()
    #past_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
    
    server_name <- "forecasts_parquet"
    
    prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]

    
  message("about to call arrow")
    past_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix)

    
  message("call completed arrow")
    
    past_forecasts <- arrow::open_dataset(past_s3) |>
      dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
      dplyr::filter(model_id == config$run_config$sim_name,
                    site_id == config$location$site_id,
                    reference_date == past_days) |>
      dplyr::collect()
    #unset_arrow_vars(vars)
  }else{
    past_forecasts <- NULL
  }

  combined_forecasts <- dplyr::bind_rows(forecast_df, past_forecasts)

  targets_df <- readr::read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)

  scoring <- generate_forecast_score_arrow(targets_df = targets_df,
                                           forecast_df = combined_forecasts, ## only works if dataframe returned from output
                                           use_s3 = config$run_config$use_s3,
                                           bucket = config$s3$scores$bucket,
                                           endpoint = config$s3$scores$endpoint,
                                           local_directory = './scores/fcre',
                                           variable_types = c("state","parameter"))

    # forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
    # start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    # restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")

    # FLAREr::update_run_config(lake_directory = lake_directory,
    #                           configure_run_file = configure_run_file,
    #                           restart_file = restart_file,
    #                           start_datetime = start_datetime,
    #                           end_datetime = NA,
    #                           forecast_start_datetime = forecast_start_datetime,
    #                           forecast_horizon = config$run_config$forecast_horizon,
    #                           sim_name = config$run_config$sim_name,
    #                           site_id = config$location$site_id,
    #                           configure_flare = config$run_config$configure_flare,
    #                           configure_obs = config$run_config$configure_obs,
    #                           use_s3 = config$run_config$use_s3,
    #                           bucket = config$s3$restart$bucket,
    #                           endpoint = config$s3$restart$endpoint,
    #                           use_https = TRUE)

    # Sys.setenv("AWS_ACCESS_KEY_ID" = var1,
    #            "AWS_SECRET_ACCESS_KEY" = var2)

    # noaa_ready <- FLAREr::check_noaa_present(lake_directory,
    #                                          configure_run_file,
    #                                          config_set_name = config_set_name)

    # reference_date <- lubridate::as_date(forecast_start_datetime)
    # s3 <- arrow::s3_bucket(bucket = glue::glue(config$s3$vera_forecasts$bucket,"/project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=inflow_gefsClimAED"),
    #                        endpoint_override = config$s3$vera_forecasts$endpoint,
    #                        anonymous = TRUE)
    # avail_dates <- gsub("reference_date=", "", s3$ls())
    #
    #
    # if(reference_date %in% avail_dates) {
    #   inflow_ready <- TRUE
    # }else{
    #   inflow_ready <- FALSE
    # }

  #}


} ## end function

