##### STATIC OCCUPANCY MODELS #####

### Run all specified static occupancy models
run_static_occupancy_models <- function(y_matrix, site_covs, effort_variable) {
  
  # UnmarkedFrame
  data <- unmarkedFrameOccu(y = y_matrix,
                            siteCovs = site_covs,
                            obsCovs = list(effort = effort_variable))
  
  # Models to run
  fm_null <- occu(~ effort ~ 1, data)
  fm_elevation <- occu(~ effort ~ elevation_scaled, data)
  fm_habitat <- occu(~ effort ~ habitat, data)
  
  list(fm_null = fm_null,
       fm_elevation = fm_elevation,
       fm_habitat = fm_habitat,
       data = data,
       site_covs = site_covs)
}


### Extract and store predictions (mean, upper, lower)
extract_static_occupancy_predictions <- function(models, grouping_days, species) {
  
  fm_null      <- models$fm_null
  fm_elevation <- models$fm_elevation
  fm_habitat   <- models$fm_habitat
  site_covs    <- models$site_covs
  
  # Null model
  pred_null <- unmarked::predict(fm_null, type = "state", newdata = data.frame(intercept = 1))
  results_null <- data.frame(model = "null",
                             term = "intercept",
                             mean = pred_null$Predicted,
                             lower = pred_null$lower,
                             upper = pred_null$upper)
  
  # Habitat model
  habitat_levels <- c("forest", "heathland", "meadow")
  newdata_habitat <- data.frame(habitat = factor(habitat_levels, levels = habitat_levels))
  pred_habitat <- unmarked::predict(fm_habitat, type = "state", newdata = newdata_habitat)
  results_habitat <- data.frame(model = "habitat",
                                term = habitat_levels,
                                mean = pred_habitat$Predicted,
                                lower = pred_habitat$lower,
                                upper = pred_habitat$upper)
  
  # Elevation
  elevation_grid_raw <- seq(1400, 2800, by = 100)
  elevation_mean <- mean(site_covs$elevation)
  elevation_sd <- sd(site_covs$elevation)
  
  newdata_elevation <- data.frame(elevation_scaled = (elevation_grid_raw - elevation_mean) / elevation_sd)
  pred_elevation <- unmarked::predict(fm_elevation, type = "state", newdata = newdata_elevation)
  results_elevation <- data.frame(model = "elevation",
                                  term = as.character(elevation_grid_raw),
                                  mean = pred_elevation$Predicted,
                                  lower = pred_elevation$lower,
                                  upper = pred_elevation$upper)
  
  # Results storage
  # return(dplyr::bind_rows(results_null, results_habitat, results_elevation) %>%
  #        dplyr::mutate(term = as.character(term)))
  
  return(dplyr::bind_rows(results_null, results_habitat, results_elevation) %>%
         dplyr::mutate(term = as.character(term),
                       grouping = grouping_days,   
                       species  = species))
  
}



### Plot results from null models
plot_null_predictions <- function(predictions, species_name, site_name, output_dir = "figures/static_occ") {

  df <- predictions %>% dplyr::filter(model == "null", species == species_name) %>%
                        dplyr::arrange(grouping)
  
  groupings <- sort(unique(df$grouping))
  n_groupings <- length(groupings)
  blue_colors <- colorRampPalette(c("#bdd7e7", "#08306b"))(n_groupings)
  names(blue_colors) <- as.character(groupings)
  
  file_name <- paste0(output_dir, "/", site_name, "_", species_name, "_null_occupancy.png")
  png(file_name, width = 1600, height = 1200, res = 200)
  
  plot(df$grouping, df$mean,
       pch  = 19, cex = 1.5,
       las = 1,
       col  = blue_colors[as.character(df$grouping)],
       ylim = c(0, 1),
       xlab = "Grouping (days)",
       ylab = "Estimated occupancy probability",
       main = paste0("Null model - ", species_name, " - ", site_name),
       xaxt = "n")
  
  axis(1, at = df$grouping, labels = df$grouping)
  
  arrows(x0 = df$grouping, y0 = df$lower,
         x1 = df$grouping, y1 = df$upper,
         angle = 90, code = 3, length = 0, lwd = 2,
         col = blue_colors[as.character(df$grouping)])
  
  dev.off()
  return(file_name)
  
}


plot_habitat_predictions <- function(predictions, species_name, site_name, output_dir = "figures/static_occ") {

  habitat_levels <- c("forest", "heathland", "meadow")
  habitat_colors <- c("forest" = "#1a5c1a",
                      "heathland" = "#4a9e4a",
                      "meadow" = "#85c985")
  groupings <- sort(unique(predictions$grouping))
  n_groupings <- length(groupings)
  gap <- 0.8   
  bloc_gap <- 2     
  
  df <- predictions %>% dplyr::filter(model == "habitat", species == species_name) %>%
                        dplyr::mutate(term = factor(term, levels = habitat_levels))
  
  positions <- data.frame()
  x_start <- 1
  for (hab in habitat_levels) {
    for (i in seq_along(groupings)) {
      positions <- dplyr::bind_rows(positions, data.frame(
        habitat = hab,
        grouping = groupings[i],
        x_pos = x_start + (i - 1) * gap
      ))
    }
    x_start <- x_start + (n_groupings - 1) * gap + bloc_gap
  }
  
  df <- df %>% dplyr::left_join(positions, by = c("term" = "habitat", "grouping"))
  
  file_name <- paste0(output_dir, "/", site_name, "_", species_name, "_habitat_occupancy.png")
  png(file_name, width = 2000, height = 1200, res = 200)
  
  plot(NULL,
       xlim = c(0.5, max(df$x_pos) + 0.5),
       ylim = c(0, 1),
       las = 1,
       xaxt = "n",
       xlab = "Habitat / Grouping (days)",
       ylab = "Estimated occupancy probability",
       main = paste0("Habitat model - ", species_name, " - ", site_name))
  
  for (hab in habitat_levels) {
    sub <- df %>% dplyr::filter(term == hab)
    points(sub$x_pos, sub$mean,
           pch = 19, cex = 1.2,
           col = habitat_colors[hab])
    arrows(x0 = sub$x_pos, y0 = sub$lower,
           x1 = sub$x_pos, y1 = sub$upper,
           angle = 90, code = 3, length = 0, lwd = 2,
           col = habitat_colors[hab])
  }
  
  bloc_centers <- positions %>% dplyr::group_by(habitat) %>%
                                dplyr::summarise(center = mean(x_pos), .groups = "drop")
  
  axis(1, at = bloc_centers$center, labels = bloc_centers$habitat, las = 1)
  
  dev.off()
  return(file_name)
  
}


plot_elevation_predictions <- function(predictions, species_name, site_name, output_dir = "figures/static_occ") {
  
  groupings <- sort(unique(predictions$grouping))
  n_groupings <- length(groupings)
  blue_colors <- colorRampPalette(c("#bdd7e7", "#08306b"))(n_groupings)
  names(blue_colors) <- as.character(groupings)
  
  df <- predictions %>% dplyr::filter(model == "elevation", species == species_name) %>%
                        dplyr::mutate(term = as.numeric(term))
  
  file_name <- paste0(output_dir, "/", site_name, "_", species_name, "_elevation_occupancy.png")
  png(file_name, width = 1600, height = 1200, res = 200)
  
  plot(NULL,
       las = 1,
       xlim = range(df$term),
       ylim = c(0, 1),
       xlab = "Elevation (m)",
       ylab = "Estimated occupancy probability",
       main = paste0("Elevation model - ", species_name, " - ", site_name))
  
  for (g in groupings) {
    sub <- df %>% dplyr::filter(grouping == g) %>% dplyr::arrange(term)
    col <- blue_colors[as.character(g)]
    lines(sub$term, sub$mean,  lwd = 2, col = col)
    lines(sub$term, sub$lower, lwd = 1, lty = 2, col = col)
    lines(sub$term, sub$upper, lwd = 1, lty = 2, col = col)
  }

  dev.off()
  return(file_name)
}