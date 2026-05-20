##### DATA FORMATTING - SELECTION - EXPLORATION #####

### Loading date files
loading_data_files <- function(path, files_pattern = NULL, required_files = NULL, delimiters = NULL) {
  
  # checking that the specified path exists
  if (!dir.exists(path)) {
    stop(paste("Path", path, "doesn't exist"))
  }
  
  # defining patterns
  if (is.null(files_pattern)) {
    
    files_pattern <- list(camera_infos_raw = "camera_infos.*\\.csv$",
                          camera_history_raw = "camera_history.*\\.csv$",
                          deepfaune_raw = "deepfaune.*\\.csv$")
  }
  
  # defining delimiters
  if (is.null(delimiters)) {
    
    delimiters <- list(camera_infos_raw = ";",
                       camera_history_raw = ";",
                       deepfaune_raw = ",")
  }
  
  # creating storage list
  load_files <- list()
  
  # loading files
  for (file_names in names(files_pattern)) {
    
    # finding files matching the specified patterns
    data_files <- list.files(path, 
                             pattern = files_pattern[[file_names]], 
                             full.names = TRUE)
    
    if (length(data_files) == 0) {
      
      if (!is.null(required_files) && file_names %in% required_files) {
        stop(paste("Required file", file_names, "not found in", path))
      }
      
      warning(paste("No file found for", file_names, "in", path, "- skipped"))
      next  
    }
    
    # ensuring there is a no double files
    if (length(data_files) > 1) {
      warning(paste(
        "Several files found for", file_names, 
        "- use of the first one:", data_files[1]
      ))
    }
    
    delim <- delimiters[[file_names]]
    if (is.null(delim)) delim <- ","
    
    load_files[[file_names]] <- readr::read_delim(data_files[1], delim = delim, show_col_types = FALSE)
  }
  
  return(load_files)
}

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
  
  return(seasonal_data_filtered)
  
}
  
  
  

  
## working on camera_infos to filter test stations, model Bushnell, and elevation below the defined limit
camtrap_infos_selection <- function(camera_infos, selected_camtrap_history, elev) {
    
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
              
               dplyr::select(station, expo, long, lat, elevation, habitat, 
                             current_model, current_PIR, 
                             detect_dist, scente, fermeture_1, fermeture_2, steep_slope) %>% 
    
               dplyr::mutate(current_model = case_when(current_model == "Moultrie800i" ~ "moultrie", # standardize camera traps' models
                                                       current_model == "Moultrie40i" ~ "moultrie",
                                                       current_model == "ReconyxHF2X" ~ "reconyx",
                                                       TRUE ~ current_model)) %>%

               dplyr::filter(elevation > elev, 
                             current_model != "Bushnell",
                             habitat != "rocky") # removing Bushnell models and stations set up to an elevation < to the lowest elevation defined as parameter, as well as rocky habitats
 
  
  return (df)

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
                      dplyr::select(station, datetime_date, datetime_time, predictionbase, scorebase) %>%
                      dplyr::filter(scorebase >= id_score)
                    
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
visualize_species_presence <- function(df, score_id, site, output_dir = "figures") {
  
  summary <- df %>% dplyr::filter(predictionbase != "empty") %>%
                    dplyr::group_by(predictionbase) %>% 
                    dplyr::summarise(total = n(), .groups = "drop") %>%
                    dplyr::mutate(prct = 100 * total / sum(total)) %>% 
                    dplyr::arrange(desc(prct)) %>% 
                    dplyr::filter(prct >= 1)
  
  file_name <- paste0(output_dir, "/", site, "_totaldetections_", score_id, ".png")
  png(filename = file_name, width = 2000, height = 1200, res = 300)
  
  colors <- hcl.colors(nrow(summary), palette = "Sunset")
  
  bp <- barplot(height = summary$prct,
                names.arg = summary$predictionbase,
                col = colors,
                main = "Total detections per species",
                ylab = "Percentage (%)",
                las = 2,        
                cex.names  = 0.8, 
                ylim = c(0, max(summary$prct) + 10))
  
  mtext(paste0("Identification score (DeepFaune) ≥ ", score_id), side = 3, line = 0, cex = 0.9, col = "black")

  dev.off()
  
  return(file_name)
  
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



plot_density_per_site_species <- function(detection_intervals_list, site, species, output_dir = "figures") {

  detection_intervals <- detection_intervals_list$intervals_station_year
  
  years_list <- split(detection_intervals, detection_intervals$year)
  cols <- viridis(length(years_list), option = "B")

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
    lines(dens_year, col = cols[i], lwd = 2)
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
    lines(dens_year, col = cols[i], lwd = 2)
    i <- i + 1
  }
  
  legend("topright",  
         legend = c("All years", names(years_list)),
         col = c("black", cols),
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



plot_rai <- function(camera_infos, detection_matrix, rai, site, species, grouping_days, start_month) {


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
  rai_plot <- rai %>% dplyr::left_join(camera_infos %>% dplyr::select(station, elevation), by = "station") %>%
                                                        dplyr::mutate(elev_category = dplyr::case_when(elevation < 2000 ~ "Low (<2000m)",
                                                                                                       elevation >= 2000 & elevation <= 2400 ~ "Mid (2000-2400m)",
                                                                                                       elevation > 2400 ~ "High (>2400m)"))
                                                      
  # Create station order based on elevation (ascending)
  station_levels <- camera_infos %>% dplyr::filter(station %in% unique(rai$station)) %>%
                                     dplyr::arrange(elevation) %>%
                                     dplyr::pull(station)
  
  
  # Apply factor with correct levels to rai_plot
  rai_plot <- rai_plot %>% dplyr::mutate(station_label = factor(station, levels = station_levels))
  
  
  # Prepare information regarding non-functioning periods of time
  non_operational <- detection_matrix %>% dplyr::mutate(year = lubridate::year(date), period = if(grouping_days == 7) {
                                                 lubridate::isoweek(date)
                                              } else {
                                                 day_since_start <- as.numeric(date - as.Date(paste0(year, "-", sprintf("%02d", start_month), "-01")))
                                                 floor(day_since_start / grouping_days)
                                              }
                                              ) %>%
                                                
                                           dplyr::filter(is.na(n_detections)) %>%
                                           dplyr::left_join(camera_infos %>% dplyr::select(station, elevation), by = "station") %>%
                                           dplyr::mutate(station_label = factor(station, levels = station_levels)) %>%
                                           dplyr::distinct(station_label, year, period)  
  
  # Plotting the plot!
  p <- ggplot(rai_plot, aes(x = period, y = station_label, fill = rai)) + 
    
              geom_tile(data = subset(rai_plot, rai > 0)) +
              scale_fill_gradientn(colours = c("lavenderblush1", "lavenderblush4", "hotpink3", "hotpink4"),
                                   limits = c(min(rai_plot$rai[rai_plot$rai > 0]), max(rai_plot$rai, na.rm = TRUE)),
                                   name = "RAI",
                                   na.value = "grey80") +
              
              geom_tile(data = non_operational,
                        aes(x = period, y = station_label, fill = NULL, color = "Non-operational"),
                        fill = "grey80",
                        alpha = 0.7,
                        linewidth = 0.1) +
              
              geom_tile(data = subset(rai_plot, rai == 0),
                        fill = "white",
                        color = "white", linewidth = 0.5) +
              
              scale_color_manual(values = c("Non-operational" = "grey30"),
                                 name = "") +
              
              facet_wrap(~ year, scales = "free_x", ncol = n_distinct(rai_plot$year)) +
              
              # Labels and titles
              labs(title = paste("Relative Abundance Index -", species, "-", site),
                   subtitle = subtitle,
                   x = x_label,
                   y = "Station (ordered by elevation)") +
              
              # Last settings
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.text.y = element_text(size = 8),
                    strip.text = element_text(face = "bold", size = 11),
                    panel.grid = element_blank(),
                    legend.position = "right",
                    plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(size = 10)) +
              
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_discrete(expand = c(0, 0))
  
  return(p)

}


save_rai_plot <- function(plot, site, species, grouping_days) {
  
  dir.create("figures/rai_plots", recursive = TRUE, showWarnings = FALSE)
  
  file_path <- paste0("figures/rai_plots/rai_", site, "_",
                                              species, "_",
                                              grouping_days, "days.png")
  
  ggplot2::ggsave(filename = file_path, plot = plot,
                                        width = 12,
                                        height = 8,
                                        dpi = 300)
  
  return(file_path)
  
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
    
    effort_full <- station_years %>% dplyr::left_join(detection_effort %>% dplyr::select(station, year, period, effort), by = c("station", "year", "period")) %>%
                                     dplyr::mutate(effort = tidyr::replace_na(effort, 0))
    
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
  
  effort[is.na(effort)] <- 0
  
  effort_scaled <- as.data.frame(matrix(scale(as.vector(as.matrix(effort))),
                                 nrow = nrow(effort),
                                 ncol = ncol(effort),
                                 dimnames = list(rownames(effort), colnames(effort))))

  
  return(effort_scaled)
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



compute_site_level_variables <- function(y_matrix, camtraps) {

  site_covs <- data.frame(station_year = rownames(y_matrix)) %>% dplyr::mutate(station = stringr::str_extract(station_year, "^[^_]+")) %>%
                                                                 dplyr::left_join(camtraps, by = "station") %>%
                                                                 dplyr::select(habitat, elevation, 
                                                                               detect_dist, fermeture_1, fermeture_2, scente, steep_slope) %>%
                                                                 dplyr::mutate(habitat = as.factor(habitat),
                                                                               detect_dist = as.factor(detect_dist),
                                                                               fermeture_1 = as.factor(fermeture_1),
                                                                               fermeture_2 = as.factor(fermeture_2),
                                                                               # scente = as.factor(scente),
                                                                               steep_slope = as.factor(steep_slope),
                                                                               elevation = as.numeric(elevation),
                                                                               elevation_scaled = as.numeric(scale(elevation)))
  
  return(site_covs)
  
}


  

