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

  data.frame(
    final_ratio = final_ratio,
    minimum_ratio = minimum_ratio,
    mean_ratio = mean_ratio,
    resistance = minimum_ratio,
    average_loss = average_loss,
    cumulative_loss = cumulative_loss,
    years_below_0_90 = years_below_threshold,
    first_year_below_0_90 = first_year_below_threshold,
    recovered_by_final_year = recovered_by_final_year,
    stringsAsFactors = FALSE
  )
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
    rows[[length(rows) + 1L]] <- data.frame(
      scenario_label = lab,
      drought_probability = unique(x$drought_probability),
      land_use_intensity = unique(x$land_use_intensity),
      n_species = nrow(x),
      mean_penalty_final_ratio = mean(x$interaction_penalty_final_ratio),
      median_penalty_final_ratio = median(x$interaction_penalty_final_ratio),
      mean_penalty_average_loss = mean(x$interaction_penalty_average_loss),
      median_penalty_average_loss = median(x$interaction_penalty_average_loss),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}
