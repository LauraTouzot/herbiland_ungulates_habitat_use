##### DATA FORMATTING - SELECTION - EXPLORATION #####

### Function to convert all columns containing dates into actual dates 
##  i.e. date_move_to | setup_date| retrieval_date | Problem..._from | Problem..._to | model..._from | model..._to | PIR..._from | PIR..._to
convert_date_columns <- function(df) {
  
  # Identify all columns that contain "date" (case insensitive) or match one of the cases above (e.g. "Problem.*(from|to)")
  date_cols <- grep("date|Problem.*(from|to)|model.*(from|to)|PIR.*(from|to)", names(df), value = TRUE, ignore.case = TRUE)
  
  # Ensure the columns exist before converting
  date_cols <- date_cols[date_cols %in% names(df)]
  
  # Convert selected columns to Date format with explicit day/month/year format
  df[date_cols] <- lapply(df[date_cols], function(x) as.Date(x, format = "%d/%m/%Y"))
  
  return(df)
  
}


### Function to reconstruct history for each station
reconstruct_history <- function(df) {
  
  # Ensure all date columns are converted to Date type 
  df <- convert_date_columns(df)
  
  # Determine the earliest start date and the latest end date across all stations
  start_date = min(df$setup_date, na.rm = TRUE)
  retrieval_col <- grep("retrieval_date", names(df), value = TRUE)[1]
  end_date <- max(df[[retrieval_col]], na.rm = TRUE)
  
  # Generate a sequence of dates for the full period of interest
  all_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Create an empty history data frame with one row per date
  station_history <- data.frame(Date = all_dates)
  
  # Identify all problem period columns dynamically
  problem_from_cols <- grep("Problem.*_from", names(df), value = TRUE)
  problem_to_cols <- gsub("_from", "_to", problem_from_cols)  # Ensure matching _to columns
  
  # Check if the problem columns exist
  if (length(problem_from_cols) == 0 || length(problem_to_cols) == 0) {
    stop("No problem period columns found. Ensure the correct columns 'Problem.*_from' and 'Problem.*_to' exist.")
  }
  
  # Initialize station history matrix with default status (e.g., active, 1)
  for (station_id in df$station) {
    station_history[[station_id]] <- rep(1, length(all_dates))
  }
  
  # Iterate over each row (station) in df
  for (i in 1:nrow(df)) {
    
    station_id <- df$station[i]
    
    # Retrieve the setup and retrieval dates for the current station
    setup_date <- df[i, "setup_date"][[1]]
    retrieval_date <- df[[retrieval_col]][i]
    
    # Set the status to 0 before the setup date and after the retrieval date
    if (!is.na(setup_date)) {
      station_history[[station_id]][station_history$Date <= as.Date(setup_date)] <- 0 
    }
    
    if (!is.na(retrieval_date)) {
      station_history[[station_id]][station_history$Date >= as.Date(retrieval_date)] <- 0
    }
    
    # Get problem periods for the current station
    for (j in seq_along(problem_from_cols)) {
      
      from_col <- problem_from_cols[j]
      to_col   <- problem_to_cols[j]
      
      # Extract actual scalar values
      from_date <- df[i, from_col][[1]]
      to_date   <- df[i, to_col][[1]]
      
      # Check that both dates exist
      if (!is.na(from_date) && !is.na(to_date)) {
        
        problem_range <- seq(from_date, to_date, by = "day")
        station_history[[station_id]][station_history$Date %in% problem_range] <- 0
        
      }
    }
    
  }
  
  station_history <- station_history %>% rename(date = Date)
  
  return(station_history)
}


### Functions to select station based on the defined criteria (some corrections might also be required to have similar formats/information (eg. types of habitats))
## working on camtrap_history to filter data across the defined period, and select stations that meet the requirements of total and consecutive working days
camtrap_history_selection <- function(camtrap_history, 
                                      start_year, end_year,
                                      start_month, end_month,
                                      min_days, min_consec,
                                      min_years) {
  
  selected_period <- camtrap_history %>% dplyr::mutate(year = lubridate::year(date), 
                                                       month = lubridate::month(date)) %>%
                                         dplyr::filter(year >= start_year, year <= end_year, 
                                                       month >= start_month, month <= end_month) %>%
                                         dplyr::select(-year, -month)
  
  # creating function to include the need for a station to have worked without interruption during a given number of days
  max_consecutive_ones <- function(x) {
    x[is.na(x)] <- 0   
    r <- rle(x == 1)
    if (any(r$values)) max(r$lengths[r$values]) else 0
  }
  
  
  max_consecutive_active_years <- function(years_active) {
    if (length(years_active) == 0) return(0)
    sorted_years <- sort(unique(years_active))
    r <- rle(diff(sorted_years) == 1)
    if (length(sorted_years) == 1) return(1)
    consec <- r$lengths[r$values] + 1  
    if (length(consec) == 0) return(1)
    max(consec)
  }
  
  station_names <- setdiff(names(selected_period), "date")
  
  keep <- sapply(station_names, function(st) {
    
    x <- selected_period[[st]]
    
    total_working_day <- sum(x == 1, na.rm = TRUE)
    longest_run <- max_consecutive_ones(x)
    
    total_working_day >= min_days && longest_run >= min_consec
  
  
  years <- lubridate::year(selected_period$date)
  
  qualifying_years <- sapply(unique(years), function(yr) {
    x_yr <- x[years == yr]
    total_yr <- sum(x_yr == 1, na.rm = TRUE)
    consec_yr <- max_consecutive_ones(x_yr)
    total_yr >= min_days && consec_yr >= min_consec
  })
  
  active_years <- unique(years)[qualifying_years]
  max_consecutive_active_years(active_years) >= min_consec_years
  
  })
  
  selected_stations <- names(keep)[keep]
  cols_to_keep <- c("date", intersect(selected_stations, names(selected_period)))
  seasonal_data_filtered <- selected_period[, cols_to_keep, drop = FALSE]
  
  # life_histories <- read.csv(file = "data/bauges/life_problems_file.csv")
  # life_histories <- life_histories %>% dplyr::select(-X)
  # 
  # all_dates <- tibble(date = seq(as.Date("2023-05-01"), as.Date("2025-10-31"), by = "day"))
  # 
  # df1_full <- all_dates %>% left_join(seasonal_data_filtered, by = "date") %>%
  #                           dplyr::mutate(dplyr::across(-date, ~ tidyr::replace_na(.x, 0)))
  # 
  # life_histories <- life_histories %>% dplyr::mutate(date = as.Date(date))
  # df2_full <- all_dates %>% left_join(life_histories, by = "date") |>
  #                           dplyr::mutate(across(-date, ~ tidyr::replace_na(.x, 0)))
  # 
  # combined <- left_join(df1_full, df2_full, by = "date")
  # seasonal_data_filtered <- combined
  
  return(seasonal_data_filtered)
  
}
  
  

  
## working on camera_infos to filter test stations, model Bushnell, and elevation below the defined limit
# also uniformizing information between study sites
# and extracting additional variables (eg. % of forested areas, distance to a slope > 40°, visibility, etc)
camtrap_infos_selection_addition <- function(camera_infos, selected_camtrap_history, visibility, refuges, skilift, elev, site) {
    
  station_names <- setdiff(names(selected_camtrap_history), "date")
  
  df <- camera_infos %>% dplyr::filter(status == "production" & station %in% station_names) # removing camera traps that were set up for tests
  
  df_sf <- sf::st_as_sf(df, coords = c("long", "lat"), crs = 4326)
  df_elev <- elevatr::get_elev_point(df_sf, src = "aws", z = 10) # compute elevation to obtain similar information between study sites
  df$elevation <- df_elev$elevation
  
  df <- df %>% dplyr::mutate(habitat = case_when(habitat == "foret" ~ "forest", # standardize habitat types
                                                 habitat == "lande" ~ "heathland",
                                                 habitat == "landes" ~ "heathland",
                                                 habitat == "pelouse" ~ "meadow",
                                                 habitat == "prairie" ~ "meadow",
                                                 habitat == "rocher" ~ "rocky",
                                                 TRUE ~ habitat)) %>%
              
               dplyr::select(station, area, expo, long, lat, elevation, habitat, 
                             current_model, current_PIR, 
                             detect_dist, scente, fermeture_1, fermeture_2, steep_slope) %>% 
    
               dplyr::mutate(current_model = case_when(current_model == "Moultrie800i" ~ "moultrie", # standardize camera traps' models
                                                       current_model == "Moultrie40i" ~ "moultrie",
                                                       current_model == "ReconyxHF2X" ~ "reconyx",
                                                       TRUE ~ current_model)) %>%

               dplyr::filter(elevation > elev, 
                             current_model != "Bushnell",
                             habitat != "rocky") # removing Bushnell models and stations set up to an elevation < to the lowest elevation defined as parameter, as well as rocky habitats
 
  
  
  ### Adding northness and slope information from MNT data
  mnt <- rast("data/MNT_AlpesFR_25m_L93.asc") 
  slope <- terra::terrain(x = mnt, v = "slope", unit = "degrees")
  aspect <- terra::terrain(x = mnt, v = "aspect", unit = "degrees")
  northness <- cos(aspect * pi/180)
  eastness <- sin(aspect * pi/180)
  steep_slope_mask <- terra::ifel(slope >= 40, 1, NA) # distance to the nearest cell with slope >= 45°
  dist_to_steep <- terra::distance(steep_slope_mask)
  
  
  df_sf <- df %>% sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  
                  sf::st_transform(crs = 2154) 
  
  df <- df %>% dplyr::mutate(slope = terra::extract(slope, df_sf)[[2]],
                             aspect = terra::extract(aspect, df_sf)[[2]],
                             northness = terra::extract(northness, df_sf)[[2]],
                             eastness = terra::extract(eastness, df_sf)[[2]],
                             dist_steep_slope_m = terra::extract(dist_to_steep, df_sf)[[2]])
  
  
  ### Computing distance to the nearest forested area
  habitats_map <- rast("data/land_cover/all_classes_mosaic_median_AlpesNord_150426_2.tif") 
  habitats_map <- terra::project(habitats_map, "EPSG:2154", method = "near")
  
  forest_mask <- habitats_map == 1
  forest_only <- terra::ifel(forest_mask, 1, NA) # distance to the nearest forest pixel
  dist_to_forest <- terra::distance(forest_only)
  
  df <- df %>% dplyr::mutate(dist_forest_m = terra::extract(dist_to_forest, df_sf)[[2]])
  
  
  ### Standardizing information regarding visibility
  default_fermeture_1 <- if (site == "mb") "closed" else "open"
  default_fermeture_2 <- if (site == "mb") "less_50_prct" else "100_prct"
  
  df <- df %>% dplyr::mutate(fermeture_1 = case_when(fermeture_1 %in% c("Semi-ferme", "semi-ferme") ~ "semi_open",
                                                     fermeture_1 == "Ouvert" ~ "open",
                                                     fermeture_1 == "Ferme" ~ "closed",
                                                     TRUE ~ default_fermeture_1),

                              detect_dist = case_when(detect_dist %in% c("10-20m") ~ "10_20m",
                                                     detect_dist %in% c("20-30m") ~ "20_30m",
                                                     detect_dist %in% c("<10m", "0-10m") ~ "less_10m",
                                                     TRUE ~ "up_30m")) %>%
  
                  dplyr::mutate(across(c(habitat, current_model, detect_dist, fermeture_1, fermeture_2), as.factor))
  
  ### Adding visibility
  visibility <- visibility %>% dplyr::select(station, dist_sentier_m, prox_sentiers_350, prop_sentier_vis) %>%
                               dplyr::rename(dist_path_m = dist_sentier_m, 
                                             prox_path_350 = prox_sentiers_350,
                                             prop_path = prop_sentier_vis) %>%
                               sf::st_drop_geometry() %>% 
                               dplyr::mutate(prop_path = tidyr::replace_na(prop_path, 0))
  
  
  ### Adding distance to nearest refuge 
  refuges_proj <- sf::st_transform(refuges, crs = 2154)
  
  refuges_proj <- refuges_proj[!sf::st_is_empty(refuges_proj), ]
  refuges_proj <- refuges_proj[sf::st_is_valid(refuges_proj), ]
  refuges_proj <- sf::st_make_valid(refuges_proj)
  refuges_proj <- refuges_proj[!sf::st_is_empty(refuges_proj), ]
  refuges_pts <- suppressWarnings(sf::st_centroid(refuges_proj))
  
  df_sf_l93 <- sf::st_transform(df_sf, crs = 2154)
  nearest_idx <- sf::st_nearest_feature(df_sf_l93, refuges_pts)
  df$dist_refuge_m <- as.numeric(sf::st_distance(df_sf_l93,
                                                 refuges_pts[nearest_idx, ],
                                                 by_element = TRUE))
  
  ### Adding distance to nearest skilift
  skilift_proj <- sf::st_transform(skilift, crs = 2154)
  skilift_proj <- skilift_proj[!sf::st_is_empty(skilift_proj), ]
  skilift_proj <- skilift_proj[sf::st_is_valid(skilift_proj), ]
  skilift_proj <- sf::st_make_valid(skilift_proj)
  skilift_proj <- skilift_proj[!sf::st_is_empty(skilift_proj), ]

  nearest_lift <- sf::st_nearest_feature(df_sf_l93, skilift_proj)
  df$dist_skilift_m <- as.numeric(sf::st_distance(df_sf_l93,
                                                  skilift_proj[nearest_lift, ],
                                                  by_element = TRUE))
  
  ### Combining in distance to nearest human feature
  df <- df %>% dplyr::mutate(dist_human_feature_m = pmin(dist_refuge_m, dist_skilift_m, na.rm = TRUE))
  
  all_infos <- left_join(df, visibility, by = "station")
  
  
  if (site == "mb") {
    df_selected <- all_infos %>% dplyr::select(station, long, lat, area,
                                               habitat,
                                               elevation,
                                               current_model, current_PIR,
                                               detect_dist, fermeture_1,
                                               aspect, slope, northness, eastness,
                                               dist_path_m, prox_path_350,
                                               dist_steep_slope_m, dist_forest_m, dist_human_feature_m)
  } else {
    df_selected <- all_infos %>% dplyr::select(station, long, lat,
                                               habitat,
                                               elevation,
                                               current_model, current_PIR,
                                               detect_dist, fermeture_1,
                                               aspect, slope, northness, eastness,
                                               dist_path_m, prox_path_350,
                                               dist_steep_slope_m, dist_forest_m, dist_human_feature_m)
  }
  
  return (df_selected)

}


select_topographic_variables <- function(selected_camtraps_infos, site) {
  
  df <- selected_camtraps_infos %>% dplyr::mutate(habitat = as.factor(habitat),
                                                  detect_dist = as.factor(detect_dist),
                                                  fermeture_1 = as.factor(fermeture_1),
                                                  slope = as.numeric(slope),
                                                  northness = as.numeric(northness))
  
  
  compute_association <- function(x, y) {
    
    ok <- complete.cases(x, y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 3) return(NA_real_)
    
    # continuous x continuous → Pearson |r|  
    if (is.numeric(x) && is.numeric(y)) {
      return(abs(cor(x, y, method = "pearson")))
    }
    
    # continuous x categorical → η
    if (is.numeric(x) && is.factor(y)) {
      mod <- aov(x ~ y)
      ss <- summary(mod)[[1]][["Sum Sq"]]
      return(sqrt(ss[1] / sum(ss)))
    }
    if (is.factor(x) && is.numeric(y)) {
      mod <- aov(y ~ x)
      ss <- summary(mod)[[1]][["Sum Sq"]]
      return(sqrt(ss[1] / sum(ss)))
    }
    
    # categorical × categorical → V of Cramér
    tab <- table(x, y)
    if (any(dim(tab) < 2)) return(NA_real_)
    chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
    k <- min(dim(tab)) - 1
    return(as.numeric(sqrt(chi2 / (sum(tab) * k))))
  }
  
  # define variables to test
  vars <- c("habitat", "detect_dist", "fermeture_1", 
            "slope", "northness", "eastness")
  
  df_check <- df %>% dplyr::select(dplyr::all_of(vars))
  
  
  # building on the correlation matrix
  n <- length(vars)
  assoc_mat <- matrix(NA, n, n, dimnames = list(vars, vars))
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        assoc_mat[i, j] <- 1
      } else if (i < j) {
        v <- compute_association(df_check[[i]], df_check[[j]])
        assoc_mat[i, j] <- v
        assoc_mat[j, i] <- v
      }
    }
  }
  
  round(assoc_mat, 2)
  
  # plotting the plot
  assoc_long <- assoc_mat %>% as.data.frame() %>%
                              tibble::rownames_to_column("var1") %>%
                              tidyr::pivot_longer(-var1, names_to = "var2", 
                                                  values_to = "association")
  
  var_labels <- c(habitat = "Habitat",
                  detect_dist = "Detection distance",
                  fermeture_1 = "Openness (level)",
                  slope = "Slope",
                  northness = "Northness")
  
  var_order <- c("northness", "slope",
                 "detect_dist", "fermeture_1",
                 "habitat")
  
  assoc_long <- assoc_mat %>% as.data.frame() %>%
                              tibble::rownames_to_column("var1") %>%
                              tidyr::pivot_longer(-var1, names_to = "var2",
                                                  values_to = "association") %>%
                          
                              dplyr::mutate(idx1 = match(var1, var_order),
                                            idx2 = match(var2, var_order)) %>%
                              dplyr::filter(idx1 < idx2) %>%
                              dplyr::select(-idx1, -idx2) %>%
                          
                              dplyr::mutate(var1 = factor(var_labels[var1], 
                                                          levels = var_labels[var_order]),
                                            var2 = factor(var_labels[var2], 
                                                          levels = var_labels[var_order]))

  
  ggplot(assoc_long, aes(x = var1, y = var2, fill = association)) +
                     geom_tile(color = "white") +
                     geom_text(aes(label = sprintf("%.2f", association)), size = 3.5) +
                     scale_fill_gradientn(colours = c("white", "#C5C8D4", "#7FA4C4", "#3D5A80", "#1F2C43"),
                                          limits = c(0, 1),
                                          name = "|r| / η / V") +
                     labs(title = "Associations between site-level variables",
                          subtitle = "Pearson |r| (num-num), η (num-cat), Cramér V (cat-cat)",
                          x = NULL, y = NULL) +
                     theme_minimal() +
                     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                           axis.text.y = element_text(size = 10),
                           panel.grid = element_blank())
  
  ggplot2::ggsave(paste0("figures/correlative_variables/associations_site_vars_", site, ".png"),
                  width = 8, height = 6, dpi = 300)
  
  

}


### Function to filter deepfaune outputs based on the defined identification score (some names' corrections might also be required) and selected stations
filter_deepfaune_outputs <- function(deepfaune, selected_camtraps, selected_camtrap_history,
                                     id_score) {
  
  # separate station, date and hours into distinct columns and convert into proper format
  # filter observations based on the defined id_score
  df <- deepfaune %>% dplyr::filter(stringr::str_count(filename, "\\d{2}-\\d{2}-\\d{2}") == 2) %>%
                      dplyr::mutate(station = stringr::str_extract(filename, "^[^_]+"),
                                    datetime_date = lubridate::ymd(stringr::str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")),
                                    datetime_time = hms::as_hms(as.character(gsub("-", ":", stringr::str_extract(filename, "\\d{2}-\\d{2}-\\d{2}(?=\\(|\\.)"))))) %>%
                      dplyr::filter(score >= id_score) %>%                 
                      dplyr::select(station, datetime_date, datetime_time, top1, score) %>%
                      dplyr::rename(scorebase = score,
                                    predictionbase = top1)
                    
  station_names <- selected_camtraps$station
  study_period <- selected_camtrap_history$date
               
  
  # clean station id that are not similar to the camera info file and filter based on selected stations
  df_corrected <- df %>% dplyr::mutate(station = case_when(station == "Blaitiere1700" ~ "blaitiere1700", # Mont-Blanc' corrections
                                                           station == "Blaitiere1900" ~ "blaitiere1900",
                                                           station == "Blaitiere2046" ~ "blaitiere2046",
                                                           station == "Blaitiere2230" ~ "blaitiere2230",
                                                           station == "Blaitiere2400" ~ "blaitiere2400",
                                                           station == "Para1400" ~ "para1400",
                                                           station == "Para1600" ~ "para1600",
                                                           station == "Para1900" ~ "para1900",
                                                           station == "Para2100" ~ "para2100",
                                                           TRUE ~ station))
  
  
  camtrap_history_long <- selected_camtrap_history %>% tidyr::pivot_longer(cols = -date,
                                                                           names_to  = "station",
                                                                           values_to = "active") %>%
                                                       dplyr::filter(active == 1) %>%  
                                                       dplyr::select(station, date)
  
  # remove deepfaune observations during periods of time when the camera traps were not working
  df_corrected <- df_corrected %>% dplyr::inner_join(camtrap_history_long %>% dplyr::select(station, date), by = c("station" = "station", "datetime_date" = "date"))
  
  deepfaune_filtered <- df_corrected %>% dplyr::mutate(predictionbase = case_when(predictionbase == "red deer" ~ "reddeer",
                                                                                  predictionbase == "roe deer" ~ "roedeer",
                                                                                  predictionbase == "wild boar" ~ "wildboar",
                                                                                  TRUE ~ predictionbase))
    
  return(deepfaune_filtered)
  
}

### Function to visualize species presence by study site - simple histograms
visualize_species_presence <- function(df, score_id, site, output_dir = "figures/general_plots") {
  
  summary <- df %>% dplyr::filter(predictionbase != "empty") %>%
                    dplyr::group_by(predictionbase) %>% 
                    dplyr::summarise(total = n(), .groups = "drop") %>%
                    dplyr::mutate(prct = 100 * total / sum(total)) %>% 
                    dplyr::arrange(desc(prct)) %>% 
                    dplyr::filter(prct >= 1)
  
  file_name <- paste0(output_dir, "/", site, "_totaldetections_", score_id, ".png")
  png(filename = file_name, width = 2000, height = 1200, res = 300)
  
  pal = ltc("gaby", 15 ,"continuous")
  
  bp <- barplot(height = summary$prct,
                names.arg = summary$predictionbase,
                col = pal,
                main = "Total detections per species",
                ylab = "Percentage (%)",
                las = 2,        
                cex.names  = 0.8, 
                ylim = c(0, max(summary$prct) + 10))
  
  mtext(paste0("Identification score (DeepFaune) ≥ ", score_id), side = 3, line = 0, cex = 0.9, col = "black")

  dev.off()
  
  return(file_name)
  
}


### Function to extract daily climate variables over the study period at the site level
create_climate_variables_matrix <- function(selected_camtraps,
                                            s2m_atmo,
                                            s2m_soil,
                                            start_year,
                                            end_year,
                                            start_month,
                                            end_month,
                                            crs_projected = 2154) {
  
  s2m_points <- s2m_atmo %>% dplyr::distinct(point_id, lon, lat, alti, slope, aspect, massif) %>%
                             dplyr::mutate(dplyr::across(c(lon, lat, alti, slope, aspect, massif),
                             ~ as.numeric(.x)))
  
  # Extracting S2M points the closest to each selected camtraps based on location, as well as elevation, aspect and slope
  stations_sf <- selected_camtraps %>% sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
                                       sf::st_transform(crs = crs_projected)
  
  s2m_sf <- s2m_points %>% sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                           sf::st_transform(crs = crs_projected)
  
  nearest_idx <- sf::st_nearest_feature(stations_sf, s2m_sf)
  
  stations_info <- selected_camtraps %>% dplyr::select(station, elevation, slope, aspect) %>%
                                         dplyr::rename(slope_stn = slope, aspect_stn = aspect) %>%
                                         dplyr::mutate(massif = s2m_points$massif[nearest_idx])
  
  # Computing circular angular distance for aspect
  # in S2M: aspect = -1 for flat ground (slope = 0)
  aspect_dist <- function(a1, a2) {
    flat <- (a1 < 0) | (a2 < 0)         
    raw  <- abs(a1 - a2) %% 360
    circ <- pmin(raw, 360 - raw)
    ifelse(flat, 0, circ)
  }
  
  # Selecting best S2M point for each cameratrap
  station_to_point <- stations_info %>% dplyr::inner_join(s2m_points, by = "massif", relationship = "many-to-many") %>%
                                        dplyr::mutate(d_alt = abs(elevation - alti),
                                                      d_aspect = aspect_dist(aspect_stn, aspect),
                                                      d_slope = abs(slope_stn - slope)) %>%
                                        dplyr::group_by(station) %>%
                                        dplyr::arrange(d_alt, d_aspect, d_slope, .by_group = TRUE) %>%
                                        dplyr::slice(1) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::transmute(station, point_id, massif,
                                                         stn_elev   = elevation,  s2m_alti   = alti,
                                                         stn_aspect = aspect_stn, s2m_aspect = aspect,
                                                         stn_slope  = slope_stn,  s2m_slope  = slope,
                                                         d_alt, d_aspect, d_slope)
  
  # Grid cameratrap × date on study period
  all_dates <- purrr::map(start_year:end_year, function(yr) {
    seq(lubridate::make_date(yr, start_month, 1),
        lubridate::make_date(yr, end_month, lubridate::days_in_month(
          lubridate::make_date(yr, end_month, 1))),
        by = "day")
  }) %>% purrr::reduce(c)
  
  grid <- tidyr::expand_grid(station = station_to_point$station,
                             date = all_dates) %>%
          dplyr::left_join(station_to_point, by = "station")
  
  # Extracting maximal daily air temperature
  relevant_points <- unique(station_to_point$point_id)
  
  s2m_atmo_daily <- s2m_atmo %>% dplyr::filter(point_id %in% relevant_points) %>%
                                 dplyr::mutate(date = lubridate::as_date(datetime)) %>%
                                 dplyr::filter(lubridate::year(date)  %in% start_year:end_year,
                                               lubridate::month(date) %in% start_month:end_month) %>%
                                 dplyr::group_by(point_id, date) %>%
                                 dplyr::summarise(tair_max = max(tair, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                 dplyr::mutate(tair_max = ifelse(is.infinite(tair_max), NA_real_, tair_max))
  
  # Extracting daily snow depth
  s2m_soil_daily <- s2m_soil %>% dplyr::filter(point_id %in% relevant_points) %>%
                                 dplyr::mutate(date = lubridate::as_date(datetime)) %>%
                                 dplyr::filter(lubridate::year(date)  %in% start_year:end_year,
                                               lubridate::month(date) %in% start_month:end_month) %>%
                                 dplyr::group_by(point_id, date) %>%
                                 dplyr::summarise(snow_depth_m = mean(sdepth, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                 dplyr::mutate(snow_depth_m = ifelse(is.nan(snow_depth_m), NA_real_, snow_depth_m))
  
  # Merge both variables
  df <- grid %>% dplyr::left_join(s2m_atmo_daily, by = c("point_id", "date")) %>%
                 dplyr::left_join(s2m_soil_daily, by = c("point_id", "date"))
  
  return(df)
}

### Functions to compute and plot time between detections and time spent in from of the camera trap at the species - study site scale 
compute_detection_intervals <- function(deepfaune_filtered, species_name, max_gap_sec) {
  
  species_data <- deepfaune_filtered %>% dplyr::filter(predictionbase == species_name)
  
  # initial check
  if (nrow(species_data) == 0) {
    warning("No data provided")
    return(NULL)
  }
  
  # DeepFaune data preparation by species, year/season and camera trap and computation of time intervals and time spent in front of camtraps
  intervals <- species_data %>% dplyr::mutate(datetime_full = lubridate::ymd_hms(paste(datetime_date, datetime_time)),
                                              year = lubridate::year(datetime_full)) %>%
                                dplyr::arrange(station, year, datetime_full) %>%
                                dplyr::group_by(station, year) %>%
                                dplyr::mutate(time_diff_sec = as.numeric(difftime(datetime_full, dplyr::lag(datetime_full), units = "secs")),
                                              obs_group = cumsum(ifelse(is.na(time_diff_sec) | time_diff_sec > max_gap_sec, 1, 0))) %>%
                                dplyr::group_by(station, year, obs_group) %>%
                                dplyr::mutate(time_spent_sec = as.numeric(difftime(max(datetime_full), min(datetime_full), units = "secs"))) %>%
                                dplyr::slice_head(n = 1) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate(time_diff_hours = time_diff_sec / 3600) %>%
                                dplyr::select(station, datetime_full, year, predictionbase, time_spent_sec, time_diff_hours)

  
  # computing and saving summary per camera trap
  summary_by_station <- intervals %>% dplyr::group_by(station) %>%
                                      dplyr::summarise(n_intervals = n(),
                                                       mean_lag_hours = mean(time_diff_hours, na.rm = TRUE),
                                                       sd_lag_hours = sd(time_diff_hours, na.rm = TRUE),
                                                       se_lag_hours = sd_lag_hours / sqrt(n_intervals),
                                                       ci_lower_95 = mean_lag_hours - 1.96 * se_lag_hours,
                                                       ci_upper_95 = mean_lag_hours + 1.96 * se_lag_hours,
                                                       .groups = "drop")
  
  return(list(intervals_station_year = intervals, summary_by_station = summary_by_station))
   
}



plot_density_per_site_species <- function(detection_intervals_list, site, species, output_dir = "figures/general_plots") {

  detection_intervals <- detection_intervals_list$intervals_station_year
  
  years_list <- split(detection_intervals, detection_intervals$year)
  pal = ltc("gaby", 8, "continuous")
  
  file_name <- paste0(output_dir, "/", site, "_", gsub(" ", "_", species), "_densities.png")
  png(filename = file_name, width = 2000, height = 1200, res = 300)
  
  par(mfrow = c(1, 2), mar = c(5,5,5,2), oma = c(0,0,2,0))
  plot(1:10, 1:10, main = "")
  plot(1:10, 10:1, main = "")
  
  dens_global <- density(detection_intervals$time_diff_hours, na.rm = TRUE)
  dens_global$y <- dens_global$y / max(dens_global$y)
  plot(dens_global,
       main = "Time between detections (hours)",
       xlab = "Hours",
       ylab = "Standardized density",
       xlim = c(0,100),
       ylim = c(0,1.1),
       col = "black", lwd = 2, las = 1, cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
  
  i <- 1
  for (yr in names(years_list)) {
    dens_year <- density(years_list[[yr]]$time_diff_hours, na.rm = TRUE)
    dens_year$y <- dens_year$y / max(dens_year$y)
    lines(dens_year, col = pal[i], lwd = 2)
    i <- i + 1
  }
  

  dens_global <- density(detection_intervals$time_spent_sec)
  dens_global$y <- dens_global$y / max(dens_global$y)
  plot(dens_global,
       main = "Time spent at station (seconds)",
       xlab = "Seconds",
       ylab = "Standardized density",
       xlim = c(0,100),
       ylim = c(0,1.1),
       col = "black", lwd = 2, las = 1, cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.8)
  
  i <- 1
  for (yr in names(years_list)) {
    dens_year <- density(years_list[[yr]]$time_spent_sec)
    dens_year$y <- dens_year$y / max(dens_year$y)
    lines(dens_year, col = pal[i], lwd = 2)
    i <- i + 1
  }
  
  legend("topright",  
         legend = c("All years", names(years_list)),
         col = c("black", pal),
         lwd = 2,
         cex = 0.5,        
         bty = "n", 
         inset = 0.02,
         y.intersp = 0.8) 
  
  mtext(paste0("Density plots – species: ", species, " – site: ", site),
        side = 3, line = 0, outer = TRUE, cex = 0.9, font = 2)
  
  dev.off()
  
  return(file_name)
  
}



create_sequences <- function(deepfaune_filtered, species_name, threshold, max_gap_sec) {
  
  species_data <- deepfaune_filtered %>% dplyr::filter(predictionbase == species_name)
  
  # initial check
  if (nrow(species_data) == 0) {
    warning(paste("No data for species:", species_name))
    return(NULL)
  }
  
  sequences <- species_data %>% dplyr::mutate(datetime_full = lubridate::ymd_hms(paste(datetime_date, datetime_time)),
                                              year = lubridate::year(datetime_full)) %>%
                                dplyr::arrange(station, year, datetime_full) %>%
                                dplyr::group_by(station) %>%
                                dplyr::mutate(time_diff_sec = as.numeric(difftime(datetime_full, dplyr::lag(datetime_full), units = "secs")),
                                              sequence_id = cumsum(ifelse(is.na(time_diff_sec) | time_diff_sec > max_gap_sec, 1, 0))) %>%
                                dplyr::group_by(station, sequence_id) %>%
                                dplyr::mutate(time_spent_sec = as.numeric(difftime(max(datetime_full), min(datetime_full), units = "secs")),
                                              time_spent_sec = ifelse(time_spent_sec == 0, 1, time_spent_sec)) %>%
                                dplyr::ungroup() %>%
                                dplyr::group_by(station, sequence_id) %>%
                                dplyr::mutate(sequence_start = min(datetime_full)) %>%
                                dplyr::mutate(n_blocks = ceiling((time_spent_sec + 1) / threshold),
                                              n_blocks = ifelse(n_blocks == 0, 1, n_blocks)) %>%
                                dplyr::rowwise() %>%
                                dplyr::mutate(blocks = list(0:(n_blocks - 1))) %>%
                                tidyr::unnest(blocks) %>%
                                dplyr::mutate(block_start = sequence_start + lubridate::seconds(blocks * seq_threshold),
                                              date = as.Date(block_start),
                                              hour = format(block_start, "%H:%M:%S")) %>%
                                dplyr::distinct(station, date, hour, .keep_all = TRUE) %>%
                                dplyr::ungroup() %>%
                                dplyr::select(station, date, hour) %>%
                                dplyr::mutate(detection = 1) %>%
                                dplyr::arrange(station, date, hour)
  
  return(sequences)
  
}



create_detection_matrix <- function(sequences, camtrap_history, selected_camtraps) {
  
  if (is.null(sequences) || nrow(sequences) == 0) {
    warning("No sequences to process")
    return(NULL)
  }
  
  valid_stations <- selected_camtraps$station
  camtrap_history_filtered <- camtrap_history %>% dplyr::select(date, dplyr::any_of(valid_stations))

  date_grid <- tibble::tibble(date = camtrap_history_filtered$date)
  
  detections_daily <- sequences %>% dplyr::filter(station %in% valid_stations) %>%
                                    dplyr::group_by(station, date) %>% 
                                    dplyr::summarise(n_detections = n(), .groups = "drop")
  
  detection_matrix <- tidyr::expand_grid(station = valid_stations,
                                         date = date_grid$date) %>%
                      dplyr::left_join(detections_daily, by = c("station", "date")) %>%
                      dplyr::mutate(n_detections = tidyr::replace_na(n_detections, 0))
  
  
  camtrap_long <- camtrap_history_filtered %>% tidyr::pivot_longer(cols = -date,
                                                                   names_to = "station",
                                                                   values_to = "operational")

  detection_nb_day <- detection_matrix %>% dplyr::left_join(camtrap_long, by = c("station", "date")) %>%
                                           dplyr::mutate(n_detections = ifelse(operational == 0 | is.na(operational), NA, n_detections)) %>%
                                           dplyr::select(station, date, n_detections)


  return(detection_nb_day)
  
} 


calculate_rai <- function(detection_nb, grouping_days, start_month) {
  
  if (is.null(detection_nb)) {
    warning("No detection matrix provided")
    return(NULL)
  }
  
  rai_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date),
                                             month = lubridate::month(date),
                                             day_since_start = as.numeric(date - as.Date(paste0(year, "-", sprintf("%02d", start_month), "-01"))))
  
  if (grouping_days == 7) {
    # for weekly grouping, consider the real week to have similar information
    rai_data <- rai_data %>% dplyr::mutate(period = lubridate::isoweek(date))
    group_var <- "period"
  } else {
    # for other groupings, consider May 1st as the beginning of the season
    rai_data <- rai_data %>% dplyr::mutate(period = floor(day_since_start / grouping_days + 1))
    group_var <- "period"
  }
  
  
  rai <- rai_data %>% dplyr::group_by(station, year, .data[[group_var]]) %>%
                      dplyr::summarise(total_detections = sum(n_detections, na.rm = TRUE),
                                       operational_days = sum(!is.na(n_detections)),
                                       rai = ifelse(operational_days > 0, total_detections / operational_days, NA),
                                       grouping_days = .env$grouping_days,  
                                       .groups = "drop") %>%
                      dplyr::filter(operational_days > 0)
  
  return(rai)
  
}



plot_rai <- function(camera_infos, detection_matrix, rai, site, species, grouping_days, start_month, max_years_per_plot = 3) {
  
  rai <- rai %>% dplyr::mutate(station = as.character(station))
  camera_infos <- camera_infos %>% dplyr::mutate(station = as.character(station))
  detection_matrix <- detection_matrix %>% dplyr::mutate(station = as.character(station))

  if (is.null(rai) || nrow(rai) == 0) {
    warning("No RAI data to plot")
    return(NULL)
  }

  # X label
  if (grouping_days == 7) {
    x_label <- "Week of year"
    subtitle <- "Grouping: 7 days (calendar weeks)"
  } else {
    x_label <- paste0(grouping_days, "-day period")
    subtitle <- paste("Grouping:", grouping_days, "days")
  }


  # Order camtraps based on elevation
  rai_plot <- rai %>% dplyr::left_join(camera_infos %>% dplyr::select(station, elevation), by = "station") 

  # Create station order based on elevation (ascending)
  station_order <- rai_plot %>% dplyr::distinct(station, elevation) %>%
                                dplyr::arrange(elevation) %>%
                                dplyr::pull(station) %>%
                                as.character()
                              
  
  # Apply factor with correct levels to rai_plot
  rai_plot <- rai_plot %>% dplyr::mutate(station_label = factor(station, levels = station_order))
  
  
  # Prepare information regarding non-functioning periods of time
  non_operational <- detection_matrix %>% dplyr::mutate(year = lubridate::year(date), 
                                          period = if(grouping_days == 7) {
                                          lubridate::isoweek(date)
                                            } else {
                                              day_since_start <- as.numeric(date - as.Date(paste0(year, "-", sprintf("%02d", start_month), "-01")))
                                              floor(day_since_start / grouping_days)
                                            }) %>%
                                          dplyr::filter(is.na(n_detections)) %>%
                                          dplyr::left_join(camera_infos %>% dplyr::select(station, elevation), by = "station") %>%
                                          dplyr::distinct(station, elevation, year, period)  
    
    non_operational <- non_operational %>% dplyr::mutate(station_label = factor(station, levels = station_order))
    
  # Common color scale limits so the fill gradient is consistent across chunks
  fill_min <- min(rai_plot$rai[rai_plot$rai > 0], na.rm = TRUE)
  fill_max <- max(rai_plot$rai, na.rm = TRUE)
  
  # Split years into chunks of at most `max_years_per_plot`
  all_years <- sort(unique(rai_plot$year))
  year_chunks <- split(all_years, ceiling(seq_along(all_years) / max_years_per_plot))
  
  # One plot per chunk
  plot_list <- lapply(year_chunks, function(yrs) {
    
    rai_sub <- rai_plot %>% dplyr::filter(year %in% yrs)
    non_op_sub <- non_operational %>% dplyr::filter(year %in% yrs)
    
    year_range_label <- if (length(yrs) == 1) as.character(yrs) else paste0(min(yrs), "-", max(yrs))
    
    # plotting the plot!
    ggplot(rai_sub, aes(x = period, y = station_label, fill = rai)) + 
      geom_tile(data = rai_sub %>% dplyr::filter(rai > 0)) +
      scale_fill_gradientn(colours = c("#BDD3CE", "#83A1C3", "#3F5671", "#013D5A"),
                           limits = c(fill_min, fill_max),
                           name = "RAI",
                           na.value = "grey80") +
      geom_tile(data = non_op_sub,
                aes(x = period, y = station_label, fill = NULL, color = "Non-operational"),
                fill = "grey80", alpha = 0.7, linewidth = 0.1) +
      geom_tile(data = rai_sub %>% dplyr::filter(rai == 0),
                fill = "white", color = "white", linewidth = 0.5) +
      scale_color_manual(values = c("Non-operational" = "grey80"), name = "") +
      facet_wrap(~ year, scales = "free_x", ncol = length(yrs)) +
      labs(title = paste("Relative Abundance Index -", species, "-", site, "(", year_range_label, ")"),
           subtitle = subtitle, x = x_label, y = "Station (ordered by elevation)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 8),
            strip.text = element_text(face = "bold", size = 11),
            panel.grid = element_blank(),
            legend.position = "right",
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(limits = factor(station_order), expand = c(0, 0))
  })
  
  # Name each plot by its year range - used later to build the filename 
  names(plot_list) <- vapply(year_chunks, function(yrs) {
    if (length(yrs) == 1) as.character(yrs) else paste0(min(yrs), "-", max(yrs))
  }, character(1))
  
  return(plot_list)

}


save_rai_plot <- function(plot_list, site, species, grouping_days) {
  
  if (is.null(plot_list) || length(plot_list) == 0) {
    warning("No plots to save")
    return(character(0))
  }
  
  dir.create("figures/rai_plots", recursive = TRUE, showWarnings = FALSE)
  
  file_paths <- vapply(names(plot_list), function(year_label) {
  
  file_path <- paste0("figures/rai_plots/rai_", site, "_",
                                              species, "_",
                                              grouping_days, "days_",
                                              year_label, ".png")
  
  ggplot2::ggsave(filename = file_path, plot = plot_list[[year_label]],
                                        width = 12,
                                        height = 8,
                                        dpi = 300)
  
  file_path
  
  }, character(1))
  
  return(unname(file_paths))
  
}


compute_detection_effort_for_month_effect <- function(detection_nb, grouping_days, start_month, end_month) {
  
  if (is.null(detection_nb)) {
    warning("No detection matrix provided")
    return(NULL)
  }
  
  # specific case of a weekly grouping
  if (grouping_days == 7) {
    
    detection_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date), period = lubridate::isoweek(date))
    
  } else { # month by month computation for grouping_days ∈ {2,3,5,6} to include the 31st 
    
    n_periods_for_month <- function(n_days_in_month, grouping_days) {
      
      floor(n_days_in_month / grouping_days) +
        ifelse(n_days_in_month %% grouping_days > 0 & n_days_in_month == 31, 0, 
               ifelse(n_days_in_month %% grouping_days > 0, 1, 0))
    }
    

    assign_period_in_month <- function(day_of_month, n_days_in_month, grouping_days) {
      
      n_complete <- floor(n_days_in_month / grouping_days)
      remainder <- n_days_in_month %% grouping_days
      p <- floor((day_of_month - 1) / grouping_days)
      if (remainder > 0) p <- pmin(p, n_complete - 1)
      return(p)
    
      }
    

    months_seq <- ((start_month - 1 + 0:11) %% 12) + 1

    ref_year <- 2000
    days_per_month <- lubridate::days_in_month(as.Date(paste0(ref_year, "-", sprintf("%02d", months_seq), "-01")))
    
    periods_per_month <- purrr::map_int(days_per_month, ~ n_periods_for_month(.x, grouping_days))
    
    cumulative_offset <- c(0, cumsum(periods_per_month[-length(periods_per_month)]))
    offset_lookup <- setNames(cumulative_offset, months_seq)
    
    detection_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date),
                                                     month = lubridate::month(date),
                                                     day_of_month = lubridate::mday(date),
                                                     n_days_in_month = lubridate::days_in_month(date),
                                                     period_in_month = purrr::pmap_int(list(day_of_month, n_days_in_month), function(d, n) assign_period_in_month(d, n, grouping_days)),
                                                     month_offset = offset_lookup[as.character(month)],
                                                     period = month_offset + period_in_month) %>%
                                       dplyr::select(-month, -day_of_month, -n_days_in_month, -period_in_month, -month_offset)
  }
  
    detection_effort <- detection_data %>% dplyr::group_by(station, year, period) %>%
                                           dplyr::summarise(effort = sum(!is.na(n_detections)),
                                                            detection = dplyr::case_when(all(is.na(n_detections)) ~ NA_real_,
                                                                                         all(n_detections == 0, na.rm = TRUE) ~ 0,
                                                                                         TRUE ~ 1),
                                           .groups = "drop")
    
    return(detection_effort)
    
}



compute_livestock_presence_for_month_effect <- function(detection_nb, grouping_days, start_month, end_month) {
  
  if (is.null(detection_nb)) {
    warning("No detection matrix provided")
    return(NULL)
  }
  
  # specific case of a weekly grouping
  if (grouping_days == 7) {
    
    detection_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date), period = lubridate::isoweek(date))
    
  } else { # month by month computation for grouping_days ∈ {2,3,5,6} to include the 31st 
    
    n_periods_for_month <- function(n_days_in_month, grouping_days) {
      
      floor(n_days_in_month / grouping_days) +
        ifelse(n_days_in_month %% grouping_days > 0 & n_days_in_month == 31, 0, 
               ifelse(n_days_in_month %% grouping_days > 0, 1, 0))
    }
    
    
    assign_period_in_month <- function(day_of_month, n_days_in_month, grouping_days) {
      
      n_complete <- floor(n_days_in_month / grouping_days)
      remainder <- n_days_in_month %% grouping_days
      p <- floor((day_of_month - 1) / grouping_days)
      if (remainder > 0) p <- pmin(p, n_complete - 1)
      return(p)
      
    }
    
    
    months_seq <- ((start_month - 1 + 0:11) %% 12) + 1
    
    ref_year <- 2000
    days_per_month <- lubridate::days_in_month(as.Date(paste0(ref_year, "-", sprintf("%02d", months_seq), "-01")))
    
    periods_per_month <- purrr::map_int(days_per_month, ~ n_periods_for_month(.x, grouping_days))
    
    cumulative_offset <- c(0, cumsum(periods_per_month[-length(periods_per_month)]))
    offset_lookup <- setNames(cumulative_offset, months_seq)
    
    detection_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date),
                                                     month = lubridate::month(date),
                                                     day_of_month = lubridate::mday(date),
                                                     n_days_in_month = lubridate::days_in_month(date),
                                                     period_in_month = purrr::pmap_int(list(day_of_month, n_days_in_month), function(d, n) assign_period_in_month(d, n, grouping_days)),
                                                     month_offset = offset_lookup[as.character(month)],
                                                     period = month_offset + period_in_month) %>%
                                      dplyr::select(-month, -day_of_month, -n_days_in_month, -period_in_month, -month_offset)
  }
  
  detection_effort <- detection_data %>% dplyr::group_by(station, year, period) %>%
                                         dplyr::summarise(effort = sum(!is.na(n_detections)),
                                                          detection = if (all(is.na(n_detections))) NA_real_ else sum(n_detections, na.rm = TRUE),
                                         .groups = "drop")
  
  return(detection_effort)
  
}







compute_detection_effort <- function(detection_nb, grouping_days, start_month, end_month) {
  
  if (is.null(detection_nb)) {
    warning("No detection matrix provided")
    return(NULL)
  }
  
  # specific case of a weekly grouping
  if (grouping_days == 7) {
    
    detection_data <- detection_nb %>%
      dplyr::mutate(year   = lubridate::year(date),
                    period = lubridate::isoweek(date))
    
  } else {
    
    ref_year <- 2000
    season_start <- as.Date(paste0(ref_year, "-", sprintf("%02d", start_month), "-01"))
    season_end <- as.Date(paste0("2000-", sprintf("%02d", end_month), "-", lubridate::days_in_month(as.Date(paste0("2000-", sprintf("%02d", end_month), "-01")))))
    season_days <- as.numeric(season_end - season_start) + 1  
    
    n_complete_groups <- floor(season_days / grouping_days)
    remainder <- season_days %% grouping_days
    
    # total number of periods
    n_periods <- if (remainder == 0) {
      n_complete_groups
    } else if (remainder == 1) {
      n_complete_groups       # if only one day left, grouping with the previous sequence
    } else {
      n_complete_groups + 1   # if > 1 day left, addition of a supplementary sequence
    }
    
    # attribution of a period number
    day_to_period <- function(day_index) {   
      p <- floor((day_index - 1) / grouping_days)
      pmin(p, n_periods - 1)   
    }
    
    all_dates <- seq(season_start, season_end, by = "day")
    day_indices <- seq_along(all_dates)           
    periods <- day_to_period(day_indices)     
    
    date_lookup <- data.frame(month_day = format(all_dates, "%m-%d"), period = periods)
    
    detection_data <- detection_nb %>% dplyr::mutate(year = lubridate::year(date),
                                                     month_day = format(date, "%m-%d"),
                                                     period = date_lookup$period[match(month_day, date_lookup$month_day)]) %>%
                                       dplyr::select(-month_day)
  }
  
  detection_effort <- detection_data %>% dplyr::group_by(station, year, period) %>%
                                         dplyr::summarise(effort = sum(!is.na(n_detections)),
                                                          detection = dplyr::case_when(all(is.na(n_detections)) ~ NA_real_,
                                                          all(n_detections == 0, na.rm = TRUE) ~ 0,
                                                          TRUE ~ 1),
                                         .groups = "drop")
  
  return(detection_effort)
}



compute_y_matrix <- function(detection_effort) {
  
  y_matrix <- detection_effort %>% dplyr::arrange(station, year) %>%                        
                                   dplyr::mutate(period_label  = sprintf("p%02d", period),
                                                 station_year  = paste0(station, "_", year)) %>%
                                   dplyr::select(station_year, period_label, detection) %>%
                                   tidyr::pivot_wider(names_from  = period_label,
                                                      values_from = detection) %>%
                                   dplyr::select(station_year, sort(setdiff(names(.), "station_year"))) %>%
                                   tibble::column_to_rownames(var = "station_year")
                                
  y_matrix <- as.matrix(y_matrix)
  
  return(y_matrix)
  
}


compute_effort_matrix <- function(detection_effort, grouping_days, start_month, end_month) {
  
  if (grouping_days == 7) {
    
    station_years <- detection_effort %>% dplyr::distinct(station, year) %>%
                                          dplyr::rowwise() %>%
                                          dplyr::mutate(all_periods = list(unique(lubridate::isoweek(seq(as.Date(paste0(year, "-", sprintf("%02d", start_month), "-01")),
                                                                                                         as.Date(paste0(year, "-", sprintf("%02d", end_month), "-",
                                                                                                         lubridate::days_in_month(as.Date(paste0(year, "-", sprintf("%02d", end_month), "-01"))))), by = "day"))))) %>%
                                          tidyr::unnest(all_periods) %>%
                                          dplyr::rename(period = all_periods) %>%
                                          dplyr::ungroup()
    
    effort_full <- station_years %>% dplyr::left_join(detection_effort %>% dplyr::select(station, year, period, effort),
                                                      by = c("station", "year", "period"))
    # Note: DO NOT replace NA by 0 here, NA MUST STAY 
    
  } else {
    
    effort_full <- detection_effort
    
  }
  
  effort <- effort_full %>% dplyr::arrange(station, year) %>%
                            dplyr::mutate(period_label = sprintf("p%02d", period),
                                          station_year = paste0(station, "_", year)) %>%
                            dplyr::select(station_year, period_label, effort) %>%
                            tidyr::pivot_wider(names_from  = period_label,
                                               values_from = effort) %>%
                            dplyr::select(station_year, sort(setdiff(names(.), "station_year"))) %>%
                            tibble::column_to_rownames(var = "station_year")
  
  # Scaling preserving NAs
  mat <- as.matrix(effort)
  scaled_vec <- as.numeric(scale(as.vector(mat)))
  scaled_mat <- matrix(scaled_vec,
                       nrow = nrow(mat),
                       ncol = ncol(mat),
                       dimnames = dimnames(mat))
  
  as.data.frame(scaled_mat)
  
}

compute_month_matrix <- function(detection_effort, grouping_days, start_month, end_month) {
  
  if (grouping_days == 7) {
    
    period_to_month <- detection_effort %>% dplyr::mutate(ref_date = as.Date(paste0(year, "-01-04")) + lubridate::weeks(period - 1),
                                                          monday = ref_date - (lubridate::wday(ref_date, week_start = 1) - 1),
                                                          season_start = as.Date(paste0(year, "-", sprintf("%02d", start_month), "-01")),
                                                          season_end = as.Date(paste0(year, "-", sprintf("%02d", end_month), "-", lubridate::days_in_month(as.Date(paste0(year, "-", sprintf("%02d", end_month), "-01"))))),
                                                          first_season_day = dplyr::case_when(monday >= season_start ~ monday,
                                                          TRUE ~ season_start),
                                                          month_name = month.name[lubridate::month(first_season_day)]) %>%
                                            dplyr::select(-ref_date, -monday, -season_start, -season_end, -first_season_day)
    
  } else {
    
    n_periods_for_month <- function(n_days_in_month, grouping_days) {
      floor(n_days_in_month / grouping_days) +
        ifelse(n_days_in_month %% grouping_days > 0 & n_days_in_month == 31, 0,
               ifelse(n_days_in_month %% grouping_days > 0, 1, 0))
    }
    
    months_seq <- ((start_month - 1 + 0:11) %% 12) + 1
    ref_year <- 2000
    days_per_month <- lubridate::days_in_month(as.Date(paste0(ref_year, "-", sprintf("%02d", months_seq), "-01")))
    periods_per_month <- purrr::map_int(days_per_month, ~ n_periods_for_month(.x, grouping_days))
    cumulative_offset <- c(0, cumsum(periods_per_month[-length(periods_per_month)]))
    
    period_ranges <- tibble::tibble(month_idx = months_seq,
                                    month_name = month.name[months_seq],
                                    p_start = cumulative_offset,
                                    p_end = cumulative_offset + periods_per_month - 1)
    
    period_to_month <- detection_effort %>% dplyr::rowwise() %>%
                                            dplyr::mutate(month_name = period_ranges$month_name[period >= period_ranges$p_start & period <= period_ranges$p_end]) %>%
                                            dplyr::ungroup()
  }
  
  month_matrix <- period_to_month %>% dplyr::arrange(station, year) %>%
                                      dplyr::mutate(station_year = paste0(station, "_", year),
                                                    period_label = sprintf("p%02d", period)) %>%
                                      dplyr::select(station_year, period_label, month_name) %>%
                                      tidyr::pivot_wider(names_from  = period_label, values_from = month_name) %>%
                                      dplyr::select(station_year, sort(setdiff(names(.), "station_year"))) %>%
                                      tibble::column_to_rownames(var = "station_year")

  month_matrix[is.na(month_matrix)] <- month.name[start_month]
  
  month_matrix <- as.matrix(month_matrix)
  
  return(month_matrix)
  
}


assign_period <- function(data, grouping_days, start_month) {
  
  if (grouping_days == 7) {
    return(data %>% dplyr::mutate(year = lubridate::year(date),
                                  period = lubridate::isoweek(date)))
  }
  
  n_periods_for_month <- function(n_days_in_month, grouping_days) {
    floor(n_days_in_month / grouping_days) +
      ifelse(n_days_in_month %% grouping_days > 0 & n_days_in_month == 31, 0,
             ifelse(n_days_in_month %% grouping_days > 0, 1, 0))
  }
  
  assign_period_in_month <- function(day_of_month, n_days_in_month, grouping_days) {
    n_complete <- floor(n_days_in_month / grouping_days)
    remainder <- n_days_in_month %% grouping_days
    p <- floor((day_of_month - 1) / grouping_days)
    if (remainder > 0) p <- pmin(p, n_complete - 1)
    p
  }
  
  months_seq <- ((start_month - 1 + 0:11) %% 12) + 1
  ref_year <- 2000
  days_per_month <- lubridate::days_in_month(as.Date(paste0(ref_year, "-", sprintf("%02d", months_seq), "-01")))
  periods_per_month <- purrr::map_int(days_per_month,
                                      ~ n_periods_for_month(.x, grouping_days))
  cumulative_offset <- c(0, cumsum(periods_per_month[-length(periods_per_month)]))
  offset_lookup <- setNames(cumulative_offset, months_seq)
  
  data %>% dplyr::mutate(year = lubridate::year(date),
                         month = lubridate::month(date),
                         day_of_month = lubridate::mday(date),
                         n_days_in_month = lubridate::days_in_month(date),
                         period_in_month = purrr::pmap_int(list(day_of_month, n_days_in_month),
                                                           function(d, n) assign_period_in_month(d, n, grouping_days)),
                         month_offset = offset_lookup[as.character(month)],
                         period = month_offset + period_in_month) %>%
          dplyr::select(-month, -day_of_month, -n_days_in_month, -period_in_month, -month_offset)
  
}


compute_climate_variable_matrix <- function(daily_climate_matrix,
                                            variable,
                                            grouping_days,
                                            start_month,
                                            end_month,
                                            scale = TRUE) {
  
  aggregated <- daily_climate_matrix %>% dplyr::filter(lubridate::month(date) >= start_month,
                                                       lubridate::month(date) <= end_month) %>%
                                          assign_period(grouping_days, start_month) %>%
                                          dplyr::group_by(station, year, period) %>%
                                          dplyr::summarise(value = mean(.data[[variable]], na.rm = TRUE),
                                                           .groups = "drop") %>%
                                          dplyr::mutate(value = ifelse(is.nan(value), NA_real_, value))
  
  wide_matrix <- aggregated %>% dplyr::arrange(station, year) %>%
                                dplyr::mutate(period_label = sprintf("p%02d", period),
                                              station_year = paste0(station, "_", year)) %>%
                                dplyr::select(station_year, period_label, value) %>%
                                tidyr::pivot_wider(names_from = period_label, values_from = value) %>%
                                dplyr::select(station_year, sort(setdiff(names(.), "station_year"))) %>%
                                tibble::column_to_rownames(var = "station_year")
  
  if (!scale) return(as.matrix(wide_matrix))

  mat <- as.matrix(wide_matrix)
  scaled_vec <- as.numeric(scale(as.vector(mat)))
  as.data.frame(matrix(scaled_vec,
                       nrow = nrow(mat), ncol = ncol(mat),
                       dimnames = dimnames(mat)))
  
}



### Reformatting matrices to have stack also by months
reshape_to_month_replicates <- function(value_matrix,
                                        month_matrix,
                                        standardize = FALSE,
                                        drop_all_na = TRUE,
                                        month_levels = month.name) {
  
    value_matrix <- as.matrix(value_matrix)
    month_matrix <- as.matrix(month_matrix)
    
    stopifnot(identical(dim(value_matrix), dim(month_matrix)),
              identical(rownames(value_matrix), rownames(month_matrix)))
    
    # long format, period order = column position (avoids "10" < "2")
    long <- tibble::tibble(station_year = rep(rownames(value_matrix), times = ncol(value_matrix)),
                           period_idx = rep(seq_len(ncol(value_matrix)), each = nrow(value_matrix)),
                           month = as.vector(month_matrix),
                           value = as.vector(value_matrix))
    
    combined <- long %>% dplyr::group_by(station_year, month) %>%
                         dplyr::arrange(period_idx, .by_group = TRUE) %>%
                         dplyr::mutate(secondary = sprintf("s%02d", dplyr::row_number())) %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(new_id = paste(station_year, month, sep = "_"),
                                       month_ord = match(month, month_levels))
    
    ordered_ids <- combined %>% dplyr::distinct(new_id, station_year, month_ord) %>%
                                dplyr::arrange(station_year, month_ord) %>%
                                dplyr::pull(new_id)
                              
    wide <- combined %>% dplyr::select(new_id, secondary, value) %>%
                         tidyr::pivot_wider(names_from = secondary, values_from = value)
    
    wide <- wide[match(ordered_ids, wide$new_id), ]
    
    result <- wide %>% tibble::column_to_rownames("new_id") %>% as.matrix()
    result <- result[, sort(colnames(result)), drop = FALSE]
    
    if (drop_all_na) {
      keep   <- apply(result, 1, function(x) !all(is.na(x)))
      result <- result[keep, , drop = FALSE]
    }
    
    if (standardize) {
      center <- mean(result, na.rm = TRUE)
      scl <- sd(result, na.rm = TRUE)
      result <- (result - center) / scl
      attr(result, "scaled:center") <- center
      attr(result, "scaled:scale") <- scl
    }
    
    return(result)
    
}



build_site_covariates_by_month <- function(reshaped_matrix, month_levels = month.name) {
  
  tibble::tibble(new_id = rownames(reshaped_matrix)) %>%
                 tidyr::extract(new_id,
                                into = c("station_year", "month"),
                                regex = "^(.*)_([A-Za-z]+)$",
                                remove = FALSE) %>%
                 dplyr::mutate(station_year = factor(station_year),
                               month = factor(month, levels = month_levels))
  

  }

  
  

compute_site_level_variables <- function(y_matrix, camtraps, species) {
  
  # distance to refuge variable chosen according to the species of interest
  dist_col <- switch(species,
                     "chamois" = "dist_steep_slope_m",
                     "reddeer" = "dist_forest_m",
                     stop("Unknow species: ", species))
  
  site_covs <- tibble::tibble(new_id = rownames(y_matrix)) %>%
               tidyr::extract(new_id,
                              into = c("station_year", "month"),
                              regex  = "^(.*)_([A-Za-z]+)$",
                              remove = FALSE) %>%
               dplyr::mutate(station = stringr::str_extract(station_year, "^[^_]+"),
                             year = stringr::str_extract(station_year, "\\d{4}$")) %>%
               dplyr::left_join(camtraps, by = "station") %>%
               dplyr::mutate(station = as.factor(station),
                             year = as.factor(year),
                             station_year = as.factor(station_year),
                             month = factor(month, levels = month.name),
                            
                             habitat = as.factor(habitat),
                             northness = as.numeric(northness),
                             northness_scaled = as.vector(scale(northness)),
                             slope = as.numeric(slope),
                             slope_scaled = as.vector(scale(slope)),
                            
                             dist_path = as.numeric(prox_path_350),
                             dist_path_scaled = as.vector(scale(dist_path)),
                             dist_human_feature = as.numeric(dist_human_feature_m),
                             dist_human_feature_scaled = as.vector(scale(dist_human_feature)),
                            
                             dist_to_refuge = as.numeric(.data[[dist_col]]),
                             dist_to_refuge_scaled = as.vector(scale(dist_to_refuge))) %>%
    
              dplyr::select(-long, -lat, -current_PIR, -current_model)
  
  # sanity check
  stopifnot(identical(site_covs$new_id, rownames(y_matrix)))
  
  site_covs
}


compute_site_coordinates <- function(y_matrix, camtraps) {
  
  if (!all(c("station", "long", "lat") %in% names(camtraps))) {
    stop("camtraps must contain columns: station, long, lat. Got: ",
         paste(names(camtraps), collapse = ", "))
  }
  
  tibble::tibble(new_id = rownames(y_matrix)) %>% dplyr::mutate(station = stringr::str_extract(new_id, "^[^_]+")) %>%
                                                  dplyr::left_join(camtraps %>% dplyr::select(station, long, lat), by = "station") %>%
                                                  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
                                                  sf::st_transform(crs = 32632) %>%
                                                  sf::st_coordinates()
  
  }


compute_detection_variables <- function(effort_matrix,
                                        temp_matrix,
                                        snow_matrix,
                                        livestock_matrix,
                                        site_covs) {
  
  ref_rows <- site_covs$new_id

  align_to_ref <- function(mat, name, fill_value = NA_real_) {

        aligned <- matrix(fill_value,
                      nrow = length(ref_rows),
                      ncol = ncol(mat),
                      dimnames = list(ref_rows, colnames(mat)))
    common <- intersect(ref_rows, rownames(mat))
    aligned[common, ] <- as.matrix(mat)[common, , drop = FALSE]
    
    missing <- setdiff(ref_rows, rownames(mat))
    if (length(missing) > 0) {
      warning(length(missing), " rows absent of ", name,
              " filled with ", fill_value, " (",
              paste(head(missing, 3), collapse = ", "),
              if (length(missing) > 3) ", ..." else "", ")")
    }
    
    aligned
  }
  
  list(
    effort     = align_to_ref(effort_matrix, "effort"),
    temp       = align_to_ref(temp_matrix,   "temp"),
    snow       = align_to_ref(snow_matrix,   "snow"),
    livestock  = align_to_ref(livestock_matrix, "livestock"),
    visibility = factor(site_covs$detect_dist,
                        levels = c("less_10m", "10_20m", "20_30m", "up_30m")),
    fermeture  = factor(site_covs$fermeture_1,
                        levels = c("open", "semi_open", "closed")),
    month      = factor(site_covs$month, levels = month.name)
  )
}

compute_spoccupancy_data <- function(y_matrix, site_covs, det_covs, coordinates_utm) {
  
  stopifnot(nrow(y_matrix) == nrow(site_covs),
            nrow(y_matrix) == length(det_covs$month),
            nrow(y_matrix) == nrow(coordinates_utm))
  
  list(y = y_matrix,
       occ.covs = site_covs,
       det.covs = det_covs,
       coords = coordinates_utm)
}


clean_spocc_data <- function(data_spOcc, occ_cov_names) {
  
  keep <- rowSums(is.na(
    data_spOcc$occ.covs[, occ_cov_names, drop = FALSE]
  )) == 0
  
  if (all(keep)) return(data_spOcc)
  
  message(sum(!keep), " sites removed (NA in occ.covs) over ",
          length(keep), ".")
  
  list(y = data_spOcc$y[keep, , drop = FALSE],
       occ.covs = data_spOcc$occ.covs[keep, , drop = FALSE],
       det.covs = lapply(data_spOcc$det.covs, function(x) {
          if (is.matrix(x))          x[keep, , drop = FALSE]
          else if (is.vector(x) || is.factor(x)) x[keep]
          else x
    }),
    coords = data_spOcc$coords[keep, , drop = FALSE]
  )
}
  
  





  

