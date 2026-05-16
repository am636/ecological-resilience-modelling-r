# Resilience metric calculations

resilience_metrics <- function(reference, projected, threshold = 0.90) {
  relative <- safe_ratio(projected$total_abundance, reference$total_abundance)
  relative_no_initial <- relative[projected$year > 0]
  final_ratio <- relative[length(relative)]
  minimum_ratio <- min(relative_no_initial, na.rm = TRUE)
  mean_ratio <- mean(relative_no_initial, na.rm = TRUE)
  average_loss <- mean(1 - relative_no_initial, na.rm = TRUE)
  cumulative_loss <- sum(1 - relative_no_initial, na.rm = TRUE)
  years_below_threshold <- sum(relative_no_initial < threshold, na.rm = TRUE)
  first_year_below_threshold <- if (any(relative_no_initial < threshold, na.rm = TRUE)) min(projected$year[projected$year > 0][relative_no_initial < threshold], na.rm = TRUE) else NA_real_
  recovered_by_final_year <- is.finite(final_ratio) && final_ratio >= threshold
  data.frame(final_ratio = final_ratio, minimum_ratio = minimum_ratio, mean_ratio = mean_ratio, resistance = minimum_ratio, average_loss = average_loss, cumulative_loss = cumulative_loss, years_below_0_90 = years_below_threshold, first_year_below_0_90 = first_year_below_threshold, recovered_by_final_year = recovered_by_final_year, stringsAsFactors = FALSE)
}

make_paired_interaction_effects <- function(metrics) {
  additive <- metrics[metrics$disturbance_type == "additive", ]
  interactive <- metrics[metrics$disturbance_type == "interactive", ]
  keys <- c("species_id", "scenario_label", "drought_probability", "land_use_intensity", "drought_seed")
  paired <- merge(additive, interactive, by = keys, suffixes = c("_additive", "_interactive"))
  paired$interaction_penalty_final_ratio <- paired$final_ratio_interactive - paired$final_ratio_additive
  paired$interaction_penalty_average_loss <- paired$average_loss_interactive - paired$average_loss_additive
  paired$interaction_penalty_cumulative_loss <- paired$cumulative_loss_interactive - paired$cumulative_loss_additive
  paired
}

summarise_interaction_effects <- function(paired, scenarios) {
  scenario_levels <- unique(scenarios$scenario_label)
  rows <- list()
  for (lab in scenario_levels) {
    x <- paired[paired$scenario_label == lab, ]
    rows[[length(rows) + 1L]] <- data.frame(scenario_label = lab, drought_probability = unique(x$drought_probability), land_use_intensity = unique(x$land_use_intensity), n_species = nrow(x), mean_penalty_final_ratio = mean(x$interaction_penalty_final_ratio), median_penalty_final_ratio = median(x$interaction_penalty_final_ratio), mean_penalty_average_loss = mean(x$interaction_penalty_average_loss), median_penalty_average_loss = median(x$interaction_penalty_average_loss), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

run_resilience_simulation <- function(n_per_group = 8, n_years = 35, seed = 42) {
  species <- simulate_species_table(n_per_group = n_per_group, seed = seed)
  scenarios <- create_disturbance_scenarios()
  metrics_list <- list()
  trajectory_list <- list()
  for (i in seq_len(nrow(species))) {
    species_row <- species[i, ]
    pars <- species_row_to_pars(species_row)
    reference <- project_population(pars, rep(0, n_years), 0, 0)
    unique_scenarios <- unique(scenarios[, c("drought_probability", "land_use_intensity", "scenario_label")])
    for (j in seq_len(nrow(unique_scenarios))) {
      scen_base <- unique_scenarios[j, ]
      drought_seed <- make_seed(species_row$species_index, scen_base$drought_probability, scen_base$land_use_intensity)
      drought_sequence <- make_drought_sequence(n_years, scen_base$drought_probability, drought_seed)
      matching <- scenarios[scenarios$scenario_label == scen_base$scenario_label, ]
      for (k in seq_len(nrow(matching))) {
        scen <- matching[k, ]
        projected <- project_population(pars, drought_sequence, scen$land_use_intensity, scen$interaction_strength)
        projected$reference_abundance <- reference$total_abundance
        projected$relative_to_reference <- safe_ratio(projected$total_abundance, reference$total_abundance)
        met <- resilience_metrics(reference, projected)
        met$species_index <- species_row$species_index
        met$species_id <- species_row$species_id
        met$life_history <- as.character(species_row$life_history)
        met$baseline_lambda <- species_row$baseline_lambda
        met$disturbance_tolerance <- species_row$disturbance_tolerance
        met$scenario_id <- scen$scenario_id
        met$scenario_label <- scen$scenario_label
        met$drought_probability <- scen$drought_probability
        met$land_use_intensity <- scen$land_use_intensity
        met$disturbance_type <- scen$disturbance_type
        met$interaction_strength <- scen$interaction_strength
        met$drought_seed <- drought_seed
        met$drought_years <- sum(drought_sequence)
        metrics_list[[length(metrics_list) + 1L]] <- met
        keep_example <- species_row$species_index %in% c(1, 8, 16, 24) && scen$scenario_label %in% c("D0.10_L0.00", "D0.30_L0.25", "D0.50_L0.50")
        if (keep_example) {
          projected$species_id <- species_row$species_id
          projected$life_history <- as.character(species_row$life_history)
          projected$scenario_id <- scen$scenario_id
          projected$scenario_label <- scen$scenario_label
          projected$disturbance_type <- scen$disturbance_type
          projected$drought_probability <- scen$drought_probability
          projected$land_use_intensity <- scen$land_use_intensity
          trajectory_list[[length(trajectory_list) + 1L]] <- projected
        }
      }
    }
  }
  metrics <- do.call(rbind, metrics_list)
  trajectories <- do.call(rbind, trajectory_list)
  metrics$life_history <- factor(metrics$life_history, levels = c("fast", "intermediate", "slow"))
  metrics$disturbance_type <- factor(metrics$disturbance_type, levels = c("additive", "interactive"))
  paired <- make_paired_interaction_effects(metrics)
  list(species = species, scenarios = scenarios, metrics = metrics, paired_interaction = paired, interaction_summary = summarise_interaction_effects(paired, scenarios), example_trajectories = trajectories, n_years = n_years)
}
