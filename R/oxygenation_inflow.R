oxygenation_inflow <- function(oxygen_data_path = './configuration/glm_aed_flare_v3_faasr_HOx_on/oxygen_data_2013_2023.csv',
                               inflow_forecast_future,
                               inflow_forecast_historic,
                               config = config,
                               use_oxygenation = TRUE){

library(dplyr)
library(readr)
library(lubridate)
library(stringr)

  ### IN THE FUTURE JUST PASS IN THE FUTURE AND HISTORIC INFLOWS, SET EVERYTHING (EXCEPT DO, FLOW, TEMP) TO ZERO, AND RETURN 2 DFs

  forecast_start_date <- as.character.Date(config$run_config$forecast_start_datetime)
  model_start_date <- as.character.Date(config$run_config$start_datetime)

  oxygen_data <- read_csv(oxygen_data_path)


  ## read in forecast of interest from S3 to grab 9m water temperature
  # forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)

  server_name <- "forecasts_parquet"
  prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
  forecast_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix=prefix)

  inflow_temp_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == 'glm_aed_flare_v3_faasr_HOx_off', ## just using test model_id, need to change this
                  site_id == config$location$site_id,
                  variable == 'Temp_C_mean',
                  reference_date == lubridate::as_datetime(reference_datetime)) |>
    dplyr::collect() |>
    filter(depth == max(depth),
           datetime == as.Date(forecast_start_date)) |>
    summarise(mean_temp = mean(prediction))



  mean_do_in <- oxygen_data |>
    filter(OXY_oxy != 0) |>
    summarise(mean_Oxy = mean(OXY_oxy)) |>
    pull(mean_Oxy)

  mean_temp_in <- inflow_temp_df$mean_temp #fill in temp with predictions from HOX off forecast
  mean_flow_in <- mean(oxygen_data$FLOW)

  ## convert inflow to be 1e-8 [m3/s] -- adjust concentration to keep same amount of mass inflow
  adjusted_flow_in <- 0.00000001 #m3/s
  mean_do_in_converted <- (mean_do_in*mean_flow_in)/adjusted_flow_in


  df_historical_period <- inflow_forecast_historic |>
    mutate(flow_number = 2) |>
    dplyr::filter(datetime >= config$run_config$start_datetime,
                  datetime < config$run_config$forecast_start_datetime)#,
                  #variable %in% c('OXY_oxy', 'FLOW', 'TEMP'))

  oxygen_inflow_historic <- df_historical_period

  oxygen_inflow_historic$prediction <- ifelse(oxygen_inflow_historic$variable == 'OXY_oxy',
                                            mean_do_in,
                                            oxygen_inflow_historic$prediction)

  oxygen_inflow_historic$prediction <- ifelse(oxygen_inflow_historic$variable == 'FLOW',
                                            mean_flow_in,
                                            oxygen_inflow_historic$prediction)

  oxygen_inflow_historic$prediction <- ifelse(oxygen_inflow_historic$variable == 'TEMP',
                                            mean_temp_in,
                                            oxygen_inflow_historic$prediction)

  oxygen_inflow_historic$prediction <- ifelse(!(oxygen_inflow_historic$variable %in% c('OXY_oxy','FLOW',"TEMP")),
                                            0,
                                            oxygen_inflow_historic$prediction)

  
  oxygen_inflow_future <- inflow_forecast_future |>
    mutate(flow_number = 2) |>
    filter(reference_datetime >= forecast_start_date)#,
           #variable %in% c('OXY_oxy', 'FLOW', 'TEMP'))

  oxygen_inflow_future$prediction <- ifelse(oxygen_inflow_future$variable == 'OXY_oxy',
                                            mean_do_in,
                                            oxygen_inflow_future$prediction)

  oxygen_inflow_future$prediction <- ifelse(oxygen_inflow_future$variable == 'FLOW',
                                            mean_flow_in,
                                            oxygen_inflow_future$prediction)

  oxygen_inflow_future$prediction <- ifelse(oxygen_inflow_future$variable == 'TEMP',
                                            mean_temp_in,
                                            oxygen_inflow_future$prediction)

  oxygen_inflow_future$prediction <- ifelse(!(oxygen_inflow_future$variable %in% c('OXY_oxy','FLOW',"TEMP")),
                                              0,
                                            oxygen_inflow_future$prediction)

  oxygenation_inflows <- list(oxygen_inflow_future, oxygen_inflow_historic)

  return(oxygenation_inflows)
}
