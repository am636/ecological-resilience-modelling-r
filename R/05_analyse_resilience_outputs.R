# Output writing

write_table <- function(x, path) {
  write.csv(x, path, row.names = FALSE)
}

write_resilience_outputs <- function(results, output_dir) {
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  write_table(results$species, file.path(table_dir, "simulated_species_traits.csv"))
  write_table(results$scenarios, file.path(table_dir, "disturbance_scenarios.csv"))
  write_table(results$metrics, file.path(table_dir, "resilience_metrics.csv"))
  write_table(results$paired_interaction, file.path(table_dir, "paired_interaction_effects.csv"))
  write_table(results$interaction_summary, file.path(table_dir, "interaction_effect_summary.csv"))
  write_table(results$example_trajectories, file.path(table_dir, "example_population_trajectories.csv"))

  run_summary <- data.frame(
    n_species = nrow(results$species),
    n_scenarios = nrow(results$scenarios),
    n_modelled_cases = nrow(results$metrics),
    n_years = results$n_years,
    min_baseline_lambda = min(results$species$baseline_lambda),
    max_baseline_lambda = max(results$species$baseline_lambda),
    min_final_ratio = min(results$metrics$final_ratio),
    max_final_ratio = max(results$metrics$final_ratio),
    mean_interaction_penalty_final_ratio = mean(results$paired_interaction$interaction_penalty_final_ratio),
    mean_interaction_penalty_average_loss = mean(results$paired_interaction$interaction_penalty_average_loss)
  )
  write_table(run_summary, file.path(table_dir, "run_summary.csv"))

  plot_resilience_outputs(results, figure_dir)
  invisible(list(table_dir = table_dir, figure_dir = figure_dir, run_summary = run_summary))
}

print_run_summary <- function(results, output_dir) {
  paired <- results$paired_interaction
  cat("Ecological resilience workflow finished.\n")
  cat("Output folder: ", output_dir, "\n", sep = "")
  cat("Species simulated: ", nrow(results$species), "\n", sep = "")
  cat("Scenarios: ", nrow(results$scenarios), "\n", sep = "")
  cat("Modelled cases: ", nrow(results$metrics), "\n", sep = "")
  cat("Baseline lambda range: ", round(min(results$species$baseline_lambda), 3), " - ", round(max(results$species$baseline_lambda), 3), "\n", sep = "")
  cat("Final ratio range: ", round(min(results$metrics$final_ratio), 3), " - ", round(max(results$metrics$final_ratio), 3), "\n", sep = "")
  cat("Mean interaction penalty in final ratio: ", round(mean(paired$interaction_penalty_final_ratio), 3), "\n", sep = "")
  cat("Mean interaction penalty in average loss: ", round(mean(paired$interaction_penalty_average_loss), 3), "\n", sep = "")
}
