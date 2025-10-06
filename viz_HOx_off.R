viz_HOx_off <- function(plots_directory = './plots/glm_aed_flare_v3_faasr_HOx_off',
                        configure_run_file = "configure_run.yml",
                        config_set_name = 'glm_aed_flare_v3_faasr_HOx_off',
                        target_url = 'https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz'){

library(dplyr)     # filter, collect, rename, left_join, join_by
library(readr)     # read_csv
library(ggplot2)   # ggplot, geom_line, geom_vline, theme_bw, etc.
library(glue)      # glue
library(stringr)   # str_split_fixed


  # Sys.setenv('AWS_ACCESS_KEY_ID' = Sys.getenv("OSN_KEY"),
  #            'AWS_SECRET_ACCESS_KEY' = Sys.getenv('OSN_SECRET'))

  ## read in config
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
  
  config <- FLAREr::set_up_simulation(configure_run_file = configure_run_file, lake_directory = lake_directory, config_set_name = config_set_name)

  #Sys.setenv('AWS_ACCESS_KEY_ID' = Sys.getenv('AWS_ACCESS_KEY_ID_FAASR'))
  #Sys.setenv('AWS_SECRET_ACCESS_KEY' = Sys.getenv('AWS_SECRET_ACCESS_KEY_FAASR'))
  #Sys.setenv("AWS_DEFAULT_REGION" = "us-west-2")

  #Sys.setenv("AWS_DEFAULT_REGION" = config$s3$set_up$region,
             #"AWS_S3_ENDPOINT" = config$s3$set_up$endpoint,
             #"USE_HTTPS" = TRUE)

  ## read in forecast from the faasr s3 location
  #faasr_forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket,
                                        #endpoint_override = config$s3$forecasts_parquet$endpoint)

  server_name <- "forecasts_parquet"
  prefix <- stringr::str_split_fixed(config$s3$forecasts_parquet$bucket, "/", n = 2)[2]
  faasr_forecast_s3 <- faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix=prefix)

  forecast_df <- arrow::open_dataset(faasr_forecast_s3) |>
    dplyr::filter(model_id == config$run_config$sim_name,
                  site_id == config$location$site_id,
                  reference_date == as.character.Date(config$run_config$forecast_start_datetime),
                  variable %in% c('Temp_C_mean', 'DO_mgL_mean')) |>
    collect()

  ### NEED TO CONVERT DO TO MG_L
  forecast_df$prediction <- ifelse(forecast_df$variable == 'DO_mgL_mean', (forecast_df$prediction/1000)*32, forecast_df$prediction)

  forecast_df <- forecast_df |>
    filter(depth == 9)

  # read targets
  targets_df <- read_csv(target_url) |>
    rename(depth = depth_m)

  ## code from flare plotting function
  site_id <- config$location$site_id
  sim_id <- config$run_config$sim_name
  reference_date <- as.Date(config$run_config$forecast_start_datetime)

  file_name <- paste0(sim_id,'_',reference_date, '.pdf')

  pdf_file_path <- file.path(plots_directory, file_name)

  if(!dir.exists(plots_directory)) {
  dir.create(plots_directory, recursive = TRUE)
  cat("Created directory:", plots_directory, "\n")
}

  combined_df <- left_join(forecast_df, targets_df, by = join_by(datetime, site_id, depth, variable)) |>
    filter(depth == 9)

  focal_depths_plotting <- unique(combined_df$depth)
  max_ensembles <- max(combined_df$parameter)

  focal_ensemebles <- 1:min(c(10, max_ensembles))

  pdf(pdf_file_path,width = 11, height = 5)

  single_ensemble <- combined_df |>
    filter(parameter %in% focal_ensemebles)

  state_plot <- combined_df |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = parameter), color = "gray") +
    geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
    #geom_point(data = obs, aes(x = datetime, y = observation), color = "red") +
    geom_vline(aes(xintercept = reference_datetime)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~variable, scales = 'free')

    plot(state_plot)

    dev.off()

    invisible(pdf_file_path)

    ## save local plot pdf to S3
    #plot_s3 <- arrow::s3_bucket(config$s3$output_plots$bucket, endpoint_override = config$s3$output_plots$endpoint)
    s3_save_path <- glue::glue('https://',config$s3$output_plots$endpoint,'/',config$s3$output_plots$bucket)
    #arrow::write_csv_arrow(pdf_file_name, sink = plot_s3$path(file_name))
    # aws.s3::put_object(file = pdf_file_path,
    #                    object = file_name,
    #                    bucket = s3_save_path)

    #minioclient::mc("cp --recursive ./plots/glm_aed_flare_v3_faasr_HOx_off/
                    #s3_faasr/faasr-bucket-0001/flare/restart/fcre/glm_aed_flare_v3_faasr_HOx_on/")

  server_name <- "output_plots"
  remote_folder <- "flare/plots/model_id=glm_aed_flare_v3_faasr_HOx_off"
  remote_file <- basename(pdf_file_path)

  pdf_file_absolute <- normalizePath(pdf_file_path, mustWork = FALSE)
  
  # Verify file exists before uploading
  if (!file.exists(pdf_file_absolute)) {
    stop(paste("PDF file was not created:", pdf_file_absolute))
  }
  
  local_folder <- dirname(pdf_file_absolute)
  local_file <- basename(pdf_file_absolute)

faasr_put_file(server_name = server_name,
                      local_folder = local_folder,
                      local_file = local_file,
                      remote_folder = remote_folder,
                      remote_file = remote_file)

}
