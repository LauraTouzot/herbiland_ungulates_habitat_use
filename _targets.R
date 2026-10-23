######## Options and packages 

# Loading targets
library(targets)
library(tarchetypes)
library(crew)

# Loading functions
lapply(list.files("R", pattern = ".R$", full.names = TRUE), source)

# Installing if needed and loading packages
packages.in <- c("dplyr", "tidyr", "tidyverse", "glue", "stringr", "lubridate", "purrr",
                 "sf", "elevatr", "terra","maps","mapdata",
                 "readr", "viridis", "ggplot2", "forcats", "ltc",
                 "unmarked", "spOccupancy", "ade4", "factoextra",
                 "furrr", "future", "crew")

for (pkg in packages.in) if(!(pkg %in% rownames(installed.packages()))) install.packages(pkg)

# Specifying target options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = packages.in, 
               memory = "transient", garbage_collection = TRUE,
               controller = crew_controller_local(workers = 6))


# Configuration of the 2 study sites
config_sites <- tibble::tibble(site = c("mb"),
                               path = c("data/montblanc"))


# Configuration of the different parameters used throughout the pipeline
identification_score <- 0.90 # DeepFaune score of identification

beginning_year = 2018 # beginning of the overall study period in 2018
ending_year = 2025 # end of the overall study period in 2025
beginning_month = 5 # beginning of the seasonal study period on May 1st
ending_month = 10 # end of the seasonal study period on October 31st
min_nb_days <- 90 # minimum nb of working days per season for a camera trap to be included
consecutive_days <- 30 # minimum nb of consecutive working days per season for a camera trap to be included
min_consec_years <- 1 # minimum nb of consecutive years of functioning for a camera to be included in the analyses


lowest_elevation <- 1390 # lowest elevation for a station to be included

species_list <- c("chamois", "reddeer") 

gap_sec = 6
seq_threshold = 60 # threshold used to define / create a sequence
# groupings_month <- c(2, 3, 5, 6, 7) # number of days used to compute visits when a month effect is included
# groupings_days <- c(2, 3, 4, 5, 6, 7) # number of days used to compute visits

groupings_month <- 3 # number of days used to compute visits when a month effect is included



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
          
          tar_target(visibility, sf::st_read(list.files(path, pattern = "freq.*\\.gpkg$", 
                                                        full.names = TRUE)[1], 
                                             quiet = TRUE)),      

          
          tar_target(refuges, sf::st_read(list.files(path, pattern = "building_refuges_OSM.*\\.geojson$", 
                                                          full.names = TRUE)[1], 
                                               quiet = TRUE)),
          
          tar_target(skilift, sf::st_read(list.files(path, pattern = "skilift_OSM.*\\.geojson$", 
                                                     full.names = TRUE)[1], 
                                          quiet = TRUE)),
          
          tar_target(s2m_atmo_file, find_file(path, "s2m_.*_atmospheric_.*\\.Rdata$"), format = "file"),
          tar_target(s2m_atmo, load_rdata(s2m_atmo_file)),
          
          tar_target(s2m_soil_file, find_file(path, "s2m_.*_soil_.*\\.Rdata$"), format = "file"),
          tar_target(s2m_soil, load_rdata(s2m_soil_file)),
          
          ### 2. Formatting data files and selecting stations / periods
          tar_target(camtrap_history, reconstruct_history(camera_history)), # creating camera traps' histories (functioning vs not functioning) based on records of problems
          
          tar_target(selected_camtrap_history, camtrap_history_selection(camtrap_history, 
                                                                         start_year = beginning_year, end_year = ending_year,
                                                                         start_month = beginning_month, end_month = ending_month,
                                                                         min_days = min_nb_days, min_consec = consecutive_days, min_years = min_consec_years)), # selecting stations using criteria fitting the research question / performed analyses - focus on operating days

          ### 3. Computing and selecting site variables, along with sites' coordinates
          tar_target(selected_camtraps, camtrap_infos_selection_addition(camera_infos, selected_camtrap_history,
                                                                         visibility, 
                                                                         refuges,
                                                                         skilift,
                                                                         elev = lowest_elevation, 
                                                                         site = site)),
          
          tar_target(selected_topo_variables, select_topographic_variables(selected_camtraps, site)),
          
          ### 4. Filtering DeepFaune outputs
          tar_target(deepfaune_filtered, filter_deepfaune_outputs(deepfaune, selected_camtraps, selected_camtrap_history, id_score = identification_score)),
          tar_target(species_detection, visualize_species_presence(deepfaune_filtered, identification_score, site), format = "file"),
          
          ### 5. Computing daily supplementary variables (eg. temperature) at the site level
          tar_target(daily_climate_matrix, create_climate_variables_matrix(selected_camtraps, s2m_atmo, s2m_soil,
                                                                           beginning_year, ending_year,
                                                                           beginning_month, ending_month)),
          
          ### 6. Computing livestock matrix
          tar_target(livestock_nb_day, create_sequences(deepfaune_filtered, 
                                                        species = c("sheep", "cow"),
                                                        threshold = seq_threshold,
                                                        max_gap_sec = gap_sec)),
          
          tar_target(livestock_nb_matrix, create_detection_matrix(livestock_nb_day, selected_camtrap_history, selected_camtraps)),
                     
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
         # tar_map(
         #    values = tibble::tibble(grouping_days = groupings_days),
         #    names = grouping_days,

          # tar_map to uncomment if month effect is required
          tar_map(
            values = tibble::tibble(grouping_month = groupings_month),
            names = grouping_month,

         # tar_target(rai_groupings, calculate_rai(detection_nb_matrix, grouping_days, beginning_month)),
         # tar_target(rai_plot_object, plot_rai(selected_camtraps, detection_nb_matrix, rai_groupings, site, species, grouping_days, beginning_month)),
         # tar_target(rai_plot_file, save_rai_plot(rai_plot_object, site, species, grouping_days), format = "file"),

         tar_target(rai_groupings, calculate_rai(detection_nb_matrix, grouping_month, beginning_month)),
         tar_target(rai_plot_object, plot_rai(selected_camtraps, detection_nb_matrix, rai_groupings, site, species, grouping_month, beginning_month)),
         tar_target(rai_plot_file, save_rai_plot(rai_plot_object, site, species, grouping_month), format = "file"),

        # computing y_matrix, site-level covariates and observation-level covariates to perform occupancy models
        # option with no month effect to include
        # tar_target(detection_effort_groupings, compute_detection_effort(detection_nb_matrix, grouping_days, beginning_month, ending_month)),
        # tar_target(y_matrix, compute_y_matrix(detection_effort_groupings)),
        # tar_target(effort_variable, compute_effort_matrix(detection_effort_groupings, grouping_days, beginning_month, ending_month)),

        # with the option of including a month effect
        tar_target(detection_effort_groupings_month_effect, compute_detection_effort_for_month_effect(detection_nb_matrix, grouping_month, beginning_month, ending_month)),
        tar_target(y_matrix_month_effect, compute_y_matrix(detection_effort_groupings_month_effect)),
        tar_target(effort_variable_month_effect, compute_effort_matrix(detection_effort_groupings_month_effect, grouping_month, beginning_month, ending_month)),
        tar_target(month_variable, compute_month_matrix(detection_effort_groupings_month_effect, grouping_month, beginning_month, ending_month)),

        tar_target(temperature_variable_month_effect, compute_climate_variable_matrix(daily_climate_matrix, "tair_max", grouping_month, beginning_month, ending_month)),
        tar_target(snow_depth_variable_month_effect, compute_climate_variable_matrix(daily_climate_matrix, "snow_depth_m", grouping_month, beginning_month, ending_month)),
        tar_target(livestock_detection_effort_month_effect, compute_livestock_presence_for_month_effect(livestock_nb_matrix, grouping_month, beginning_month, ending_month)),
        tar_target(livestock_variable_month_effect, compute_y_matrix(livestock_detection_effort_month_effect)),

        # reshaping matrices stacked by month
        tar_target(y_by_month_replicate, reshape_to_month_replicates(y_matrix_month_effect, month_variable)),
        tar_target(effort_by_month_replicate, reshape_to_month_replicates(effort_variable_month_effect, month_variable)),
        tar_target(temperature_by_month_replicate, reshape_to_month_replicates(temperature_variable_month_effect, month_variable)),
        tar_target(snow_depth_by_month_replicate, reshape_to_month_replicates(snow_depth_variable_month_effect, month_variable)),
        tar_target(livestock_by_month_replicate, reshape_to_month_replicates(livestock_variable_month_effect, month_variable)),

        # recreating "month" and "station_year" site covariates
        tar_target(site_covariates_by_month, build_site_covariates_by_month(y_by_month_replicate)),

        # computing data into the appropriate format to run occupancy models using spOccupancy
        tar_target(site_covs_pgocc, compute_site_level_variables(y_by_month_replicate,
                                                                 selected_camtraps,
                                                                 species)),

        tar_target(det_covs_pgocc,
                   compute_detection_variables(effort_matrix = effort_by_month_replicate,
                                               temp_matrix = temperature_by_month_replicate,
                                               snow_matrix = snow_depth_by_month_replicate,
                                               livestock_matrix = livestock_by_month_replicate,
                                               site_covs = site_covs_pgocc)),
        tar_target(coords_pgocc,
                   compute_site_coordinates(y_by_month_replicate,
                                            selected_camtraps)),

        tar_target(data_pgocc,
                   compute_spoccupancy_data(y_by_month_replicate,
                                            site_covs_pgocc,
                                            det_covs_pgocc,
                                            coords_pgocc)),

        tar_target(data_pgocc_clean,
                   clean_spocc_data(data_pgocc,
                                    occ_cov_names = c("habitat",
                                                      "northness_scaled",
                                                      "slope_scaled",
                                                      "dist_path_scaled",
                                                      "dist_human_feature_scaled",
                                                      "dist_to_refuge_scaled")))
        # tar_target(pgocc_fit_mod1,
        #            fit_pgocc_model_mod1(data_pgocc_clean,
        #                                 n_samples = 10000,
        #                                 n_burn    = 5000,
        #                                 n_chains  = 3))
            ) # tar_map grouping_days
          ) # tar_map species
        ) # tar_map sites
  ) # overall target list