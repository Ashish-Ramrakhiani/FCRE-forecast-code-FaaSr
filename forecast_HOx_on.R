##  FUNCTION TO RUN FLARE FORECAST WITH NO ADDITIONAL OXYGENATION
## author: Austin Delany
## last modified: 2025-06-09

forecast_HOx_on <- function(configure_run_file = "configure_run.yml",
                            config_set_name = 'glm_aed_flare_v3_faasr_HOx_on'){


  library(dplyr)
  library(lubridate)
  library(readr)

  
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

  #config$run_config$sim_name <- config_set_name (MIGHT WANT TO UPDATE SIM NAME FOR THIS SCENARIO)

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

  server_name <- "vera_forecasts"
  prefix <- stringr::str_split_fixed(config$s3$vera_forecasts$bucket, "/", n = 2)[2]
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

  forecast_start_date <- as.character.Date(config$run_config$forecast_start_datetime)
  model_start_date <- as.character.Date(config$run_config$start_datetime)



  ###############  HISTORICAL INFLOW (WILL JUST BE GRABBED FROM S3 IN THE FUTURE) #################
  #df_historical_period <- arrow::open_dataset(arrow::s3_bucket(bucket = glue::glue(config$s3$inflow_drivers$bucket,'/',config$flows_s3$historical_inflow_model),
                                                             #endpoint_override = config$s3$inflow_drivers$endpoint,
                                                             #anonymous = TRUE)) |>

  print(paste("made it till this inflow drivers call"))

  server_name <- "inflow_drivers"
  prefix <- glue::glue(stringr::str_split_fixed(config$s3$inflow_drivers$bucket, "/", n = 2)[2], "/", config$flows_s3$historical_inflow_model)
  hist_inflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                               faasr_prefix = prefix)
  df_historical_period <- arrow::open_dataset(hist_inflow_s3) |>


    # dplyr::filter(reference_date >= model_start_date,
    #               reference_date <= forecast_start_date,#as.character.Date(config$run_config$forecast_start_datetime)
    #               model_id == 'inflow_gefsClimAED_HOx_off') |> ##
    dplyr::collect()


  ###############  FUTURE INFLOW (WILL JUST BE GRABBED FROM S3 IN THE FUTURE) #################

  #df_future_period <- arrow::open_dataset(arrow::s3_bucket(bucket = glue::glue(config$s3$inflow_drivers$bucket,'/',config$flows_s3$future_inflow_model),
                                                             #endpoint_override = config$s3$inflow_drivers$endpoint,
                                                             #anonymous = TRUE)) |>

  print(paste("made it till this call"))

  server_name <- "inflow_drivers"
  prefix <- glue::glue(stringr::str_split_fixed(config$s3$inflow_drivers$bucket, "/", n = 2)[2], "/", config$flows_s3$future_inflow_model)
  future_inflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                 faasr_prefix = prefix)
  df_future_period <- arrow::open_dataset(future_inflow_s3) |>

    # dplyr::filter(reference_date >= model_start_date,
    #               reference_date <= forecast_start_date)#as.character.Date(config$run_config$forecast_start_datetime)) |> ##
    dplyr::collect()

  ## add oxygenation flow
  source(file.path(lake_directory, "R", "oxygenation_inflow.R"))

  message("=== DEBUG: About to call oxygenation_inflow ===")
print(paste("df_future_period rows:", nrow(df_future_period)))
print(paste("df_historical_period rows:", nrow(df_historical_period)))
print(paste("Config object type:", class(config)))
  
  oxygenation_df <- oxygenation_inflow(oxygen_data_path = './configuration/glm_aed_flare_v3_faasr_HOx_on/oxygen_data_2013_2023.csv',
                     inflow_forecast_future = df_future_period,
                     inflow_forecast_historic = df_historical_period,
                     config = config,
                     use_oxygenation = TRUE)

  message("=== DEBUG: About to call oxygenation_inflow ===")
print(paste("df_future_period rows:", nrow(df_future_period)))
print(paste("df_historical_period rows:", nrow(df_historical_period)))
print(paste("Config object type:", class(config)))

  ## combine weir inflow with oxygenation flow
  flow_combined_future <- bind_rows(df_future_period,oxygenation_df[[1]]) |>
    dplyr::mutate(model_id = 'inflow_gefsClimAED_HOx_on',
                  site_id = 'fcre',
                  reference_date = as.Date(reference_datetime))

  flow_combined_historic <- bind_rows(df_historical_period,oxygenation_df[[2]]) |>
    mutate(model_id = 'historical_interp_inflow_HOx_on',
           site_id = 'fcre')

  ## save combined inflows locally
  message('saving inflow forecasts locally...')
  arrow::write_dataset(flow_combined_future,
                       path = file.path(config$flows$local_inflow_directory,'future'),
                       partitioning = c("model_id", "reference_date", "site_id"))

  arrow::write_dataset(flow_combined_historic,
                       path = file.path(config$flows$local_inflow_directory,'historic'),
                       partitioning = c("model_id", "site_id"))


  ## read in outflow forecasts and save locally
  #df_historic_outflow <- arrow::open_dataset(arrow::s3_bucket(bucket = glue::glue(config$s3$outflow_drivers$bucket,'/',config$flows_s3$historical_outflow_model),
                                                           #endpoint_override = config$s3$inflow_drivers$endpoint,
                                                           #anonymous = TRUE)) |>

  server_name <- "outflow_drivers"
  prefix <- glue::glue(stringr::str_split_fixed(config$s3$outflow_drivers$bucket, "/", n = 2)[2], "/", config$flows_s3$historical_outflow_model)
  hist_outflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                faasr_prefix = prefix)
  df_historic_outflow <- arrow::open_dataset(hist_outflow_s3) |>

    dplyr::collect() |>
    mutate(site_id = 'fcre',
           model_id = 'historical_interp_outflow_HOx_on')


 # df_future_outflow <- arrow::open_dataset(arrow::s3_bucket(bucket = glue::glue(config$s3$outflow_drivers$bucket,'/',config$flows_s3$future_outflow_model),
                                                          # endpoint_override = config$s3$inflow_drivers$endpoint,
                                                           # anonymous = TRUE)) |>

  server_name <- "outflow_drivers"
  prefix <- glue::glue(stringr::str_split_fixed(config$s3$outflow_drivers$bucket, "/", n = 2)[2], "/", config$flows_s3$future_outflow_model)
  future_outflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                  faasr_prefix = prefix)
  df_future_outflow <- arrow::open_dataset(future_outflow_s3) |>

    dplyr::collect() |>
    mutate(site_id = 'fcre',
           reference_date = as.Date(reference_datetime),
           model_id = 'outflow_gefsClimAED_HOx_on')


  message('saving outflow forecasts locally...')
  arrow::write_dataset(df_future_outflow,
                       path = file.path(config$flows$local_outflow_directory,'future'),
                       partitioning = c("model_id", "reference_date", "site_id"))

  arrow::write_dataset(df_historic_outflow,
                       path = file.path(config$flows$local_outflow_directory,'historic'),
                       partitioning = c("model_id", "site_id"))


  readr::read_csv(config$targets$vera_insitu_targets, show_col_types = FALSE) |>
    dplyr::mutate(observation = ifelse(variable == "DO_mgL_mean", observation*1000*(1/32), observation),
                  observation = ifelse(variable == "fDOM_QSU_mean", -151.3407 + observation*29.62654, observation),
                  depth_m = ifelse(depth_m == 0.1, 0.0, depth_m)) |>
    dplyr::rename(depth = depth_m) |>
    dplyr::filter(site_id == "fcre",
                  datetime >= as_datetime(config$run_config$start_datetime)) |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
    readr::write_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")))


  # ## SET CONFIGURATIONS FOR THIS RUN (use local flow drivers and use separate sim name for running and storing forecasts)
  # forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  # start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  # restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)), "-",config$run_config$sim_name ,".nc")
  # #restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)), "-",'glm_aed_flare_v3_faasr_HOx_on',".nc")
  #
  # # make restart folder for HOx on folder
  # # restart_directory_scenario <- file.path(lake_directory, "restart",
  # #                                         config$location$site_id,
  # #                                         scenario_sim_names$sim_name[j])
  # #
  # # dir.create(restart_directory_scenario, recursive = TRUE, showWarnings = FALSE)
  #
  #
  # ## make local version of new restart config
  # config_set_name = 'glm_aed_flare_v3_faasr_HOx_on'
  # config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file, lake_directory = lake_directory, config_set_name = config_set_name)
  #
  # FLAREr::update_run_config(lake_directory = lake_directory,
  #                           configure_run_file = configure_run_file,
  #                           restart_file = restart_file,
  #                           start_datetime = start_datetime,
  #                           end_datetime = NA,
  #                           forecast_start_datetime = forecast_start_datetime,
  #                           forecast_horizon = config$run_config$forecast_horizon,
  #                           sim_name = 'glm_aed_flare_v3_faasr_HOx_on',
  #                           site_id = config$location$site_id,
  #                           configure_flare = config$run_config$configure_flare,
  #                           configure_obs = config$run_config$configure_obs,
  #                           use_s3 = config$run_config$use_s3,
  #                           bucket = config$s3$restart$bucket,
  #                           endpoint = config$s3$restart$endpoint,
  #                           config = config,
  #                           use_https = TRUE)
  #
  # config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file, lake_directory = lake_directory, config_set_name = config_set_name, clean_start = TRUE)
  # config$flows$use_flows_s3 <- FALSE

  FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  ## SCORE FORECASTS -- ADD SCORES TO FLARE bucket
  message("Scoring forecasts")
  source('./R/generate_forecast_score_arrow.R')

  #forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
  server_name <- "forecasts_parquet"
  prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
  forecast_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name, faasr_prefix=prefix)
  forecast_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == config$run_config$sim_name,
                  site_id == config$location$site_id,
                  reference_date == lubridate::as_datetime(config$run_config$forecast_start_datetime)) |>
    dplyr::collect()

  if(config$output_settings$evaluate_past & config$run_config$use_s3){
    #past_days <- lubridate::as_date(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon)
    past_days <- lubridate::as_date(lubridate::as_date(config$run_config$forecast_start_datetime) - lubridate::days(config$run_config$forecast_horizon))

    #vars <- arrow_env_vars()
    #past_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
    server_name <- "forecasts_parquet"
    prefix <- stringr::str_split_fixed(config$s3$vera_forecasts$bucket, "/", n = 2)[2]
    past_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix=prefix)

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

  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)

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
  #
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
  #
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
  #
  # #}


} ## end function

