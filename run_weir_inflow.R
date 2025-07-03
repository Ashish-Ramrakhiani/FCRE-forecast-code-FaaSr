### PURPOSE OF THIS CODE IS TO PROIVDE A STEPPING STONE FOR FULL FLARE INTEGRATION INTO FAASR.
## FOLLOW THE CONCEPTUAL DIAGRAM THAT WAS MADE DURING ROL MEETING

## FOCUS ON MAKING FLARE COMPLETELY FULLY DEPENDENT ON S3 ACROSS FUNCTIONS (ASSUME THAT NO OUTPUT FROM FUNCTIONS WILL BE SAVED LOCALLY)
## S3 PATHS CAN BE SPECIFIED IN THE FLARE CONFIG
## USE S3 PATHS AS THEY ARE IN THE CONFIG, BUT CAN ALSO ACCESS FAASR SPECIFIC CONFIGS WITH [config$faasr...]


run_weir_inflow <- function(configure_run_file = "configure_run.yml",
                            config_set_name = 'glm_aed_flare_v3_faasr_HOx_off',
                            #vera_insitu_targets_s3 = 'https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz',
                            original_inflow_model = 'inflow_gefsClimAED',
                            inflow_model_id = "inflow_gefsClimAED_HOx_off",
                            outflow_model_id = "outflow_gefsClimAED_HOx_off",
                            historic_inflow_model_id = "historical_interp_inflow_HOx_off",
                            historic_outflow_model_id = "historical_interp_outflow_HOx_off"){

  library(tidyverse)
  library(arrow)

  #Sys.setenv('AWS_ACCESS_KEY_ID' = Sys.getenv('AWS_ACCESS_KEY_ID_FAASR'))
  #Sys.setenv('AWS_SECRET_ACCESS_KEY' = Sys.getenv('AWS_SECRET_ACCESS_KEY_FAASR'))
  #Sys.setenv("AWS_DEFAULT_REGION" = "us-west-2")

  lake_directory <- here::here()
  config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name)

  print('read VERA targets...')

  targets_vera <- readr::read_csv(config$targets$vera_inflow_targets,
                                  show_col_types = FALSE)

  inflow_hist_dates <- tibble(datetime = seq(min(targets_vera$datetime), max(targets_vera$datetime), by = "1 day"))

  variables <- c("datetime", "FLOW", "TEMP", "SALT",
                 'OXY_oxy',
                 'CAR_dic',
                 'CAR_ch4',
                 'SIL_rsi',
                 'NIT_amm',
                 'NIT_nit',
                 'PHS_frp',
                 'OGM_doc',
                 'OGM_docr',
                 'OGM_poc',
                 'OGM_don',
                 'OGM_donr',
                 'OGM_pon',
                 'OGM_dop',
                 'OGM_dopr',
                 'OGM_pop',
                 'PHY_cyano',
                 'PHY_green',
                 'PHY_diatom')

  df <- targets_vera |>
    filter(!variable %in% c("DN_mgL_sample", "DC_mgL_sample")) |>
    select(datetime, variable, observation) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    right_join(inflow_hist_dates, by = "datetime") |>
    mutate(across(Flow_cms_mean:DIC_mgL_sample, imputeTS::na_interpolation)) |>
    tidyr::fill(Flow_cms_mean:DIC_mgL_sample, .direction = "up") |>
    tidyr::fill(Flow_cms_mean:DIC_mgL_sample, .direction = "down") |>
    dplyr::rename(TEMP = Temp_C_mean,
                  FLOW = Flow_cms_mean) |>
    dplyr::mutate(NIT_amm = NH4_ugL_sample*1000*0.001*(1/18.04),
                  NIT_nit = NO3NO2_ugL_sample*1000*0.001*(1/62.00), #as all NO2 is converted to NO3
                  PHS_frp = SRP_ugL_sample*1000*0.001*(1/94.9714),
                  OGM_doc = DOC_mgL_sample*1000*(1/12.01)* 0.10,  #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
                  OGM_docr = 1.5*DOC_mgL_sample*1000*(1/12.01)* 0.90, #assuming 90% of total DOC is in recalcitrant DOC pool
                  TN_ugL = TN_ugL_sample*1000*0.001*(1/14),
                  TP_ugL = TP_ugL_sample*1000*0.001*(1/30.97),
                  OGM_poc = 0.1*(OGM_doc+OGM_docr), #assuming that 10% of DOC is POC (Wetzel page 755
                  OGM_don = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.10, #DON is ~5x greater than PON (Wetzel page 220)
                  OGM_donr = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.90, #to keep mass balance with DOC, DONr is 90% of total DON
                  OGM_pon = (1/6)*(TN_ugL_sample-(NIT_amm+NIT_nit)), #detemined by subtraction
                  OGM_dop = 0.3*(TP_ugL_sample-PHS_frp)*0.10, #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
                  OGM_dopr = 0.3*(TP_ugL_sample-PHS_frp)*0.90,#to keep mass balance with DOC & DON, DOPr is 90% of total DOP
                  OGM_pop = TP_ugL_sample-(OGM_dop+OGM_dopr+PHS_frp), # #In lieu of having the adsorbed P pool activated in the model, need to have higher complexed P
                  CAR_dic = DIC_mgL_sample*1000*(1/52.515),
                  OXY_oxy = rMR::Eq.Ox.conc(TEMP, elevation.m = 506, #creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
                                            bar.press = NULL, bar.units = NULL,
                                            out.DO.meas = "mg/L",
                                            salinity = 0, salinity.units = "pp.thou"),
                  OXY_oxy = OXY_oxy *1000*(1/32),
                  CAR_ch4 = CH4_umolL_sample,
                  PHY_cyano = 0,
                  PHY_green = 0,
                  PHY_diatom = 0,
                  SIL_rsi = DRSI_mgL_sample*1000*(1/60.08),
                  SALT = 0) |>
    dplyr::mutate_if(is.numeric, round, 4) |>
    dplyr::select(dplyr::any_of(variables)) |>
    tidyr::pivot_longer(-c("datetime"), names_to = "variable", values_to = "observation") |>
    dplyr::select(datetime, variable, observation) |>
    dplyr::rename(prediction = observation) |>
    dplyr::mutate(model_id = historic_inflow_model_id) |>
    dplyr::mutate(parameter = 1,
                  flow_number = 1)

  print('finished inflow conversions...')

  if(max(df$datetime) != lubridate::as_date(config$run_config$forecast_start_datetime) - lubridate::days(1)){

    variables <- unique(df$variable)
    full_time <- seq(min(df$datetime), lubridate::as_datetime(config$run_config$forecast_start_datetime), by = "1 day")

    full_data <- list()

    for(i in 1:length(variables)){

      new_data <- tibble(datetime = full_time,
                         variable = rep(variables[i], length(full_time)),
                         parameter = 1,
                         flow_number = 1)

      full_data <- bind_rows(full_data, new_data)

    }

    df <- df |>
      dplyr::right_join(full_data, by = join_by(datetime, variable, parameter, flow_number)) |>
      dplyr::arrange(variable, parameter, flow_number, datetime) |>
      dplyr::group_by(parameter, flow_number, variable) |>
      tidyr::fill(prediction, .direction = "down") |>
      dplyr::mutate(model_id = historic_inflow_model_id,
                    site_id = config$location$site_id) |>
      dplyr::ungroup()

  }

  df_historical_inflow <- df |>
    dplyr::filter(datetime >= lubridate::as_datetime(config$run_config$start_datetime),
                  datetime < lubridate::as_datetime(config$run_config$forecast_start_datetime))

  print('saving historical inflow forecasts for FLARE...')

  server_name <- "inflow_drivers"
  prefix <- stringr::str_split_fixed(config$flows_save$historical_inflow_model, "/", n = 2)[2]
  hist_inflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                               faasr_prefix = prefix,
                                               faasr_config = config$faasr)
  arrow::write_dataset(df_historical_inflow,
                     path = hist_inflow_s3,
                     partitioning = c("model_id", "site_id"))

  #arrow::write_dataset(df_historical_inflow,
                       #path = arrow::s3_bucket(bucket = config$flows_save$historical_inflow_model,
                                               #endpoint_override = config$flows_save$endpoint),
                       #partitioning = c("model_id", "site_id"))

  #arrow::write_dataset(df, path = file.path(lake_directory, "drivers/inflow/historical/model_id=historical_interp_inflow/site_id=fcre"))

  print('saving historical outflow forecasts for FLARE...')

  df_historical_outflow <- df_historical_inflow |>
    dplyr::filter(variable %in% c("TEMP", "FLOW")) |>
    dplyr::mutate(model_id = historic_outflow_model_id)

  server_name <- "outflow_drivers"
  prefix <- stringr::str_split_fixed(config$flows_save$historical_outflow_model, "/", n = 2)[2]
  hist_outflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                faasr_prefix = prefix,
                                                faasr_config = config$faasr)
arrow::write_dataset(df_historical_outflow,
                     path = hist_outflow_s3,
                     partitioning = c("model_id", "site_id"))

  # arrow::write_dataset(df_historical_outflow,
  #                      path = arrow::s3_bucket(bucket = config$flows_save$historical_outflow_model,
  #                                              endpoint_override =  config$flows_save$endpoint),
  #                      partitioning = c("model_id", "site_id"))

  #inflow_forecast_dir <- "inflow"
  #print(inflow_forecast_dir)

  #convert_vera4cast_inflow(reference_date = lubridate::as_date(config$run_config$forecast_start_datetime),
                           #model_id = "inflow_gefsClimAED",
                           #save_bucket = future_inflow_write_bucket,
                           #save_endpoint = future_inflow_write_endpoint)
  #print('inflows converted...')

  message('Generating future forecasts...')


  inflow_variables <- c("TP_ugL_sample", "NH4_ugL_sample","NO3NO2_ugL_sample",
                 "SRP_ugL_sample","DOC_mgL_sample","DRSI_mgL_sample",
                 "TN_ugL_sample", "CH4_umolL_sample", "DIC_mgL_sample",
                 "Flow_cms_mean", "Temp_C_mean")

  forecast_df <- NULL
  reference_date = lubridate::as_date(config$run_config$forecast_start_datetime)

  for(i in 1:length(inflow_variables)){
    print(i)

    # s3 <- arrow::s3_bucket(bucket = glue::glue(config$s3$vera_forecasts$bucket,"/project_id=vera4cast/duration=P1D/variable={inflow_variables[i]}/model_id={original_inflow_model}/reference_date={reference_date}"),
    #                        endpoint_override = config$s3$vera_forecasts$endpoint,
    #                        anonymous = TRUE)

    

    vera_base_path <- stringr::str_split_fixed(config$s3$vera_forecasts$bucket, "/", n = 2)[2]

    server_name <- "vera_forecasts"
    prefix <- glue::glue("{vera_base_path}/project_id=vera4cast/duration=P1D/variable={inflow_variables[i]}/model_id={original_inflow_model}/reference_date={reference_date}")

    print(paste("Bucket base:", config$s3$vera_forecasts$bucket))
    print(paste("Vera base path:", vera_base_path))
    print(paste("Full prefix:", prefix))
    print(paste("Server name:", server_name))
    
    # Check if the server exists in config
    if (server_name %in% names(config$faasr$DataStores)) {
      print("Server found in DataStores")
      server_config <- config$faasr$DataStores[[server_name]]
      print(paste("Server endpoint:", server_config$Endpoint))
      print(paste("Server bucket:", server_config$Bucket))
      print(paste("Server region:", server_config$Region))
    } else {
      print("WARNING: Server not found in DataStores!")
    }

    print(config$faasr$DataStores$vera_forecasts)

    s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                       faasr_prefix = prefix,
                                       faasr_config = config$faasr)

    vera_path <- glue::glue("project_id=vera4cast/duration=P1D/variable={inflow_variables[i]}/model_id={original_inflow_model}/reference_date={reference_date}")


    ## test to see if inflow forecast exists ##
    tryCatch({
      df <- arrow::open_dataset(s3) |>
        dplyr::filter(site_id == "tubr") |>
        dplyr::collect() |>
        dplyr::mutate(variable = inflow_variables[i])
    }, error = function(e) {
      print("=== DETAILED ERROR INFORMATION ===")
      print(paste("Error class:", class(e)))
      print(paste("Error message:", e$message))
      
      # Print the full error object
      print("Full error object:")
      print(e)
      
      # Print call stack if available
      if (!is.null(e$call)) {
        print("Error call:")
        print(e$call)
      }
      
      # Print trace if available
      if (!is.null(e$trace)) {
        print("Error trace:")
        print(e$trace)
      }
      
      # Try to get more details about what specifically failed
      print("\n=== ATTEMPTING STEP-BY-STEP DEBUGGING ===")
      
      # Test 1: Can we create the S3 connection?
      tryCatch({
        print("Step 1: Testing S3 connection creation...")
        s3_test <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                               faasr_prefix = prefix,
                                               faasr_config = config$faasr)
        print("S3 connection created successfully")
        
        # Test 2: Can we open the dataset?
        tryCatch({
          print("Step 2: Testing dataset opening...")
          dataset_test <- arrow::open_dataset(s3_test)
          print("Dataset opened successfully")
          print(paste("Schema:", paste(names(dataset_test$schema), collapse = ", ")))
          
          # Test 3: Can we list some data?
          tryCatch({
            print("Step 3: Testing data listing...")
            sample_data <- dataset_test |> 
              dplyr::slice_head(n = 5) |> 
              dplyr::collect()
            print("Sample data retrieved successfully")
            print(paste("Sample data columns:", paste(names(sample_data), collapse = ", ")))
            print(paste("Unique site_ids in sample:", paste(unique(sample_data$site_id), collapse = ", ")))
            
            # Test 4: Can we filter by site_id?
            tryCatch({
              print("Step 4: Testing site_id filtering...")
              filtered_data <- dataset_test |>
                dplyr::filter(site_id == "tubr") |>
                dplyr::collect()
              print(paste("Filtered data rows:", nrow(filtered_data)))
              
              if (nrow(filtered_data) == 0) {
                print("WARNING: No data found for site_id 'tubr'")
                print("Available site_ids:")
                all_sites <- dataset_test |> 
                  dplyr::select(site_id) |> 
                  dplyr::distinct() |> 
                  dplyr::collect()
                print(unique(all_sites$site_id))
              }
              
            }, error = function(e4) {
              print(paste("Step 4 failed - site_id filtering error:", e4$message))
            })
            
          }, error = function(e3) {
            print(paste("Step 3 failed - data listing error:", e3$message))
          })
          
        }, error = function(e2) {
          print(paste("Step 2 failed - dataset opening error:", e2$message))
        })
        
      }, error = function(e1) {
        print(paste("Step 1 failed - S3 connection error:", e1$message))
      })
      
      # Now throw the original error with more context
      stop(paste('\nDetailed error for variable:', inflow_variables[i],
                 '\nPath:', vera_path,
                 '\nOriginal error:', e$message,
                 '\nStopping workflow...'))
    })
    
    forecast_df <- dplyr::bind_rows(forecast_df, df)
}


  FLARE_VARS <- c("parameter", "datetime", "FLOW", "TEMP", "SALT",
            'OXY_oxy',
            'CAR_dic',
            'CAR_ch4',
            'SIL_rsi',
            'NIT_amm',
            'NIT_nit',
            'PHS_frp',
            'OGM_doc',
            'OGM_docr',
            'OGM_poc',
            'OGM_don',
            'OGM_donr',
            'OGM_pon',
            'OGM_dop',
            'OGM_dopr',
            'OGM_pop',
            'PHY_cyano',
            'PHY_green',
            'PHY_diatom')

  glm_df_inflow <- forecast_df |>
    dplyr::select(datetime, variable, prediction, parameter) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(TEMP = Temp_C_mean,
                  FLOW = Flow_cms_mean) |>
    dplyr::mutate(NIT_amm = NH4_ugL_sample*1000*0.001*(1/18.04),
                  NIT_nit = NO3NO2_ugL_sample*1000*0.001*(1/62.00), #as all NO2 is converted to NO3
                  PHS_frp = SRP_ugL_sample*1000*0.001*(1/94.9714),
                  OGM_doc = DOC_mgL_sample*1000*(1/12.01)* 0.10,  #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
                  OGM_docr = 1.5*DOC_mgL_sample*1000*(1/12.01)* 0.90, #assuming 90% of total DOC is in recalcitrant DOC pool
                  TN_ugL = TN_ugL_sample*1000*0.001*(1/14),
                  TP_ugL = TP_ugL_sample*1000*0.001*(1/30.97),
                  OGM_poc = 0.1*(OGM_doc+OGM_docr), #assuming that 10% of DOC is POC (Wetzel page 755
                  OGM_don = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.10, #DON is ~5x greater than PON (Wetzel page 220)
                  OGM_donr = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.90, #to keep mass balance with DOC, DONr is 90% of total DON
                  OGM_pon = (1/6)*(TN_ugL_sample-(NIT_amm+NIT_nit)), #detemined by subtraction
                  OGM_dop = 0.3*(TP_ugL_sample-PHS_frp)*0.10, #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
                  OGM_dopr = 0.3*(TP_ugL_sample-PHS_frp)*0.90,#to keep mass balance with DOC & DON, DOPr is 90% of total DOP
                  OGM_pop = TP_ugL_sample-(OGM_dop+OGM_dopr+PHS_frp), # #In lieu of having the adsorbed P pool activated in the model, need to have higher complexed P
                  CAR_dic = DIC_mgL_sample*1000*(1/52.515),
                  OXY_oxy = rMR::Eq.Ox.conc(TEMP, elevation.m = 506, #creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
                                            bar.press = NULL, bar.units = NULL,
                                            out.DO.meas = "mg/L",
                                            salinity = 0, salinity.units = "pp.thou"),
                  OXY_oxy = OXY_oxy *1000*(1/32),
                  CAR_ch4 = CH4_umolL_sample,
                  PHY_cyano = 0,
                  PHY_green = 0,
                  PHY_diatom = 0,
                  SIL_rsi = DRSI_mgL_sample*1000*(1/60.08),
                  SALT = 0) |>
    dplyr::mutate_if(is.numeric, round, 4) |>
    dplyr::select(dplyr::any_of(FLARE_VARS)) |>
    tidyr::pivot_longer(-c("datetime","parameter"), names_to = "variable", values_to = "prediction") |>
    dplyr::mutate(datetime = lubridate::as_date(datetime),
                  model_id = inflow_model_id,
                  site_id = "fcre",
                  family = "ensemble",
                  flow_number = 1,
                  reference_datetime = lubridate::as_datetime(reference_date),
                  reference_date = as.character(reference_date)) |>
    dplyr::select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_number, reference_date)

  message('saving future inflow forecast...')

  server_name <- "inflow_drivers"
  prefix <- stringr::str_split_fixed(config$flows_save$future_inflow_model, "/", n = 2)[2]
  future_inflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                   faasr_prefix = prefix,
                                                   faasr_config = config$faasr)
  arrow::write_dataset(glm_df_inflow,
                       path = future_inflow_s3,
                       partitioning = c("model_id", "reference_date", "site_id"))

  # arrow::write_dataset(glm_df_inflow,
  #                      path = arrow::s3_bucket(bucket = config$flows_save$future_inflow_model,
  #                                              endpoint_override =  config$flows_save$endpoint),
  #                      partitioning = c("model_id", "reference_date", "site_id"))

  glm_df_outflow <- glm_df_inflow |>
    dplyr::select(datetime, parameter, variable, prediction) |>
    dplyr::filter(variable %in% c("FLOW","TEMP")) |>
    dplyr::mutate(datetime = lubridate::as_date(datetime),
                  model_id = outflow_model_id,
                  site_id = "fcre",
                  family = "ensemble",
                  flow_number = 1,
                  reference_datetime = lubridate::as_datetime(reference_date),
                  reference_date = as.character(reference_date)) |>
    dplyr::select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_number, reference_date)

  message('saving future outflow forecast...')

  server_name <- "outflow_drivers"
  prefix <- stringr::str_split_fixed(config$flows_save$future_outflow_model, "/", n = 2)[2]
  future_outflow_s3 <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,
                                                    faasr_prefix = prefix,
                                                    faasr_config = config$faasr)
  arrow::write_dataset(glm_df_outflow,
                       path = future_outflow_s3,
                       partitioning = c("model_id", "reference_date", "site_id"))

  # arrow::write_dataset(glm_df_outflow,
  #                      path = arrow::s3_bucket(bucket = config$flows_save$future_outflow_model,
  #                                              endpoint_override =  config$flows_save$endpoint),
  #                      partitioning = c("model_id", "reference_date", "site_id"))
}
