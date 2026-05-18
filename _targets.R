######## Options and packages 

# Loading targets
library(targets)
library(tarchetypes)

# Loading functions
lapply(list.files("R", pattern = ".R$", full.names = TRUE), source)

# Installing if needed and loading packages
packages.in <- c("dplyr", "tidyr", "tidyverse", "glue", "stringr", "lubridate", "purrr",
                 "sf", "elevatr", "terra","maps","mapdata",
                 "readr", "viridis", "ggplot2", "forcats",
                 "unmarked")

for (pkg in packages.in) if(!(pkg %in% rownames(installed.packages()))) install.packages(pkg)

# Specifying target options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = packages.in, 
               memory = "transient", garbage_collection = TRUE)


# Configuration of the 3 study sites
config_sites <- tibble::tibble(site = c("mb", "bel"),
                               path = c("data/montblanc/", "data/belledonne"))


# Configuration of the different parameters used throughout the pipeline
identification_score <- 0.98 # DeepFaune score of identification

beginning_year = 2018 # beginning of the overall study period in 2018
ending_year = 2025 # end of the overall study period in 2025
beginning_month = 5 # beginning of the seasonal study period on May 1st
ending_month = 10 # end of the seasonal study period on October 31st
min_nb_days <- 90 # minimum nb of working days per season for a camera trap to be included
consecutive_days <- 30 # minimum nb of consecutive working days per season for a camera trap to be included

lowest_elevation <- 1390 # lowest elevation for a station to be included

species_list <- c("chamois", "ibex", "reddeer", "roedeer") # list of species of interest for the analyses
# min_detect_year <- 500 # minimum nb of detection for a species per year in a given study site to be included in the analyses
min_consec_years <- 2 # minimum nb of consecutive years for a species with the minimum nb of detections in a given study site to be included in the analyses

gap_sec = 6
seq_threshold = 60 # threshold used to define / create a sequence
groupings_month <- c(2, 3, 5, 6, 7) # number of days used to compute visits
groupings_days <- c(2, 3, 4, 5, 6, 7) # number of days used to compute visits

min_detection <- 0.3

proportion_of_sites <- seq(0.3, 1.0, 0.1)
nb_of_years <- seq(2, 8, 1)
nb_main_repetitions <- 100
nb_sub_repetitions <- 20

# Configuration of the targets pipeline
list(
  
  tar_target(sites_config, config_sites),
  tar_target(species_to_analyze, species_list),
  
  tar_map(values = config_sites,
          names = site,
          
          ### 1. Loading all required data files (raw version)
          tar_target(camera_infos, loading_data_files(path, 
                                                      files_pattern = list(camera_infos_raw = "camera_infos.*\\.csv$"),
                                                      delimiters = list(camera_infos_raw = ";"))$camera_infos_raw),
          
          tar_target(camera_history, loading_data_files(path, 
                                                        files_pattern = list(camera_history_raw = "camera_history.*\\.csv$"),
                                                        delimiters = list(camera_history_raw = ";"))$camera_history_raw),
          
          tar_target(deepfaune, loading_data_files(path, 
                                                   files_pattern = list(deepfaune_raw = "deepfaune.*\\.csv$"),
                                                   delimiters = list(deepfaune_raw = ","))$deepfaune_raw),
          
          
          ### 2. Formatting data files and selecting stations / periods
          tar_target(camtrap_history, reconstruct_history(camera_history)), # creating camera traps' histories (functioning vs not functioning) based on records of problems
          
          tar_target(selected_camtrap_history, camtrap_history_selection(camtrap_history, 
                                                                         start_year = beginning_year, end_year = ending_year,
                                                                         start_month = beginning_month, end_month = ending_month,
                                                                         min_days = min_nb_days, min_consec = consecutive_days, min_years = min_consec_years)), # selecting stations using criteria fitting the research question / performed analyses - focus on operating days
          
          tar_target(selected_camtraps, camtrap_infos_selection(camera_infos, selected_camtrap_history,
                                                                     elev = lowest_elevation)),
          
          tar_target(deepfaune_filtered, filter_deepfaune_outputs(deepfaune, selected_camtraps, selected_camtrap_history, id_score = identification_score)),
          tar_target(species_detection, visualize_species_presence(deepfaune_filtered, identification_score, site), format = "file"),
          
          
          tar_map(
            values = tibble::tibble(species = species_list),
            names = species,

          
          ### 3. Computing exploratory metrics and associated figures for each studied species (change parameter accordingly)
          tar_target(detection_intervals, compute_detection_intervals(deepfaune_filtered, species, max_gap_sec = gap_sec)),
          tar_target(intervals_summary, detection_intervals$summary_by_station),
          tar_target(save_summary, {
            readr::write_csv(detection_intervals$summary_by_station, paste0("outputs/intervals_summary_", species, "_", site, ".csv"))
          }),
          
          tar_target(density_plots_detection, plot_density_per_site_species(detection_intervals, site, species), format = "file"),
          
          tar_target(detection_nb_day, create_sequences(deepfaune_filtered, species, threshold = seq_threshold, max_gap_sec = gap_sec)),
          tar_target(detection_nb_matrix, create_detection_matrix(detection_nb_day, selected_camtrap_history, selected_camtraps)),
          
          # computing and plotting RAI
          tar_map(
            values = tibble::tibble(grouping_days = groupings_days),
            names = grouping_days,
            
            ## tar_map to uncomment if month effect is required
            # tar_map(
            #   values = tibble::tibble(grouping_month = groupings_month),
            #   names = grouping_month,
            
            tar_target(rai_groupings, calculate_rai(detection_nb_matrix, grouping_days, beginning_month)),
            tar_target(rai_plot_object, plot_rai(selected_camtraps, detection_nb_matrix, rai_groupings, site, species, grouping_days, beginning_month)),
            tar_target(rai_plot_file, save_rai_plot(rai_plot_object, site, species, grouping_days), format = "file"),
            
            # tar_target(rai_groupings, calculate_rai(detection_nb_matrix, grouping_month, beginning_month)),
            # tar_target(rai_plot_object, plot_rai(selected_camtraps, detection_nb_matrix, rai_groupings, site, species, grouping_month, beginning_month, ending_month)),
            # tar_target(rai_plot_file, save_rai_plot(rai_plot_object, site, species, grouping_month), format = "file"),
            
            
            # computing y_matrix, site-level covariates and observation-level covariates to perform occupancy models 
            # option with no month effect to include
            tar_target(detection_effort_groupings, compute_detection_effort(detection_nb_matrix, grouping_days, beginning_month, ending_month)),
            tar_target(y_matrix, compute_y_matrix(detection_effort_groupings)),
            tar_target(effort_variable, compute_effort_matrix(detection_effort_groupings, grouping_days, beginning_month, ending_month)),
            
            # # with the option of including a month effect
            # tar_target(detection_effort_groupings_month_effect, compute_detection_effort_for_month_effect(detection_nb_matrix, groupings_month, beginning_month, ending_month)),
            # tar_target(y_matrix_month_effect, compute_y_matrix_month_effect(detection_effort_groupings_month_effect)),
            # tar_target(effort_variable_month_effect, compute_effort_matrix(detection_effort_groupings_month_effect, groupings_month, beginning_month, ending_month)),
            # tar_target(month_variable, compute_month_matrix(detection_effort_groupings_month_effect, grouping_month, beginning_month, ending_month)),
            
            
            # running static occupancy models
            tar_target(site_covs, compute_site_level_variables(y_matrix, selected_camtraps)),
            tar_target(static_occupancy_models, run_static_occupancy_models(y_matrix, site_covs, effort_variable)),
            tar_target(current_grouping_days, grouping_days),
            tar_target(current_species, species),
            tar_target(static_occupancy_predictions, extract_static_occupancy_predictions(static_occupancy_models, current_grouping_days, current_species))
            ) # tar_map grouping_days
          ), # tar_map species
          
          ## list must be adapted to the species and grouping days included
          ## if a month effect is included, do not forget to remove all models with a 4-days grounping window from the last
          tar_target(all_static_occupancy_predictions, dplyr::bind_rows(static_occupancy_predictions_2_chamois,
                                                                        static_occupancy_predictions_3_chamois,
                                                                        static_occupancy_predictions_4_chamois,
                                                                        static_occupancy_predictions_5_chamois,
                                                                        static_occupancy_predictions_6_chamois,
                                                                        static_occupancy_predictions_7_chamois,

                                                                        static_occupancy_predictions_2_ibex,
                                                                        static_occupancy_predictions_3_ibex,
                                                                        static_occupancy_predictions_4_ibex,
                                                                        static_occupancy_predictions_5_ibex,
                                                                        static_occupancy_predictions_6_ibex,
                                                                        static_occupancy_predictions_7_ibex,

                                                                        static_occupancy_predictions_2_reddeer,
                                                                        static_occupancy_predictions_3_reddeer,
                                                                        static_occupancy_predictions_4_reddeer,
                                                                        static_occupancy_predictions_5_reddeer,
                                                                        static_occupancy_predictions_6_reddeer,
                                                                        static_occupancy_predictions_7_reddeer,

                                                                        static_occupancy_predictions_2_roedeer,
                                                                        static_occupancy_predictions_3_roedeer,
                                                                        static_occupancy_predictions_4_roedeer,
                                                                        static_occupancy_predictions_5_roedeer,
                                                                        static_occupancy_predictions_6_roedeer,
                                                                        static_occupancy_predictions_7_roedeer)),

          
          tar_target(plot_null_mb, purrr::map_chr(species_list, ~ plot_null_predictions(all_static_occupancy_predictions, 
                                   .x, 
                                   site_name  = "MontBlanc")),
                                   format = "file"),
          
          tar_target(plot_habitat_mb, purrr::map_chr(species_list, ~ plot_habitat_predictions(all_static_occupancy_predictions, 
                                      .x, 
                                      site_name  = "MontBlanc")),
                                      format = "file"),

          tar_target(plot_elevation_mb, purrr::map_chr(species_list, ~ plot_elevation_predictions(all_static_occupancy_predictions, 
                                                       .x, 
                                                       site_name = "MontBlanc")),
                                                       format = "file")

  ) # tar_map sites
) # overall target list 