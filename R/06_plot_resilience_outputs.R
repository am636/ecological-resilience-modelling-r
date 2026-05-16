# Plotting functions

plot_box <- function(data, response, group, path, title, ylab, xlab) {
  png(path, width = 1200, height = 850, res = 120)
  op <- par(mar = c(7, 6, 4, 2), cex.axis = 0.95, cex.lab = 1.05)
  on.exit({par(op); dev.off()}, add = TRUE)
  groups <- unique(data[[group]])
  values <- lapply(groups, function(g) data[data[[group]] == g, response])
  boxplot(values, names = groups, main = title, ylab = ylab, xlab = xlab, las = 1, border = "grey35", col = c("#d8c2b2", "#c8d8e8", "#c8dcc0"), lwd = 1.2, medlwd = 2.4)
}

plot_final_ratio_by_disturbance <- function(metrics, path) {
  png(path, width = 1700, height = 950, res = 130)
  op <- par(mfrow = c(1, 2), mar = c(8, 6, 4, 2), oma = c(3, 0, 0, 0), cex.axis = 0.9, cex.lab = 1.0)
  on.exit({par(op); dev.off()}, add = TRUE)
  labels <- unique(metrics$scenario_label)
  cols <- rep(c("#c8d8e8", "#c8dcc0", "#d8c2b2"), each = 3)
  for (tp in c("additive", "interactive")) {
    x <- metrics[metrics$disturbance_type == tp, ]
    vals <- lapply(labels, function(z) x[x$scenario_label == z, "final_ratio"])
    boxplot(vals, names = gsub("_", "\n", labels), ylim = c(0, 1.05), las = 2, main = paste("Disturbance type:", tp), ylab = "Final abundance relative to no-disturbance reference", xlab = "", col = cols, border = "grey35", lwd = 1.1, medlwd = 2.4)
    abline(h = 1, lty = 2, col = "grey45")
    abline(v = c(3.5, 6.5), lty = 3, col = "grey80")
  }
  mtext("Scenario (D = drought probability, L = land-use intensity)", side = 1, outer = TRUE, line = 1)
}

plot_interaction_penalty <- function(paired, path, response, title, ylab) {
  png(path, width = 1200, height = 780, res = 120)
  op <- par(mar = c(8, 6, 4, 2), cex.axis = 0.9, cex.lab = 1.05)
  on.exit({par(op); dev.off()}, add = TRUE)
  labels <- unique(paired$scenario_label)
  vals <- lapply(labels, function(z) paired[paired$scenario_label == z, response])
  cols <- rep(c("#c8d8e8", "#c8dcc0", "#d8c2b2"), each = 3)
  ylim <- range(unlist(vals), 0, na.rm = TRUE)
  boxplot(vals, names = gsub("_", "\n", labels), ylim = ylim, las = 1, main = title, ylab = ylab, xlab = "Scenario (D = drought probability, L = land-use intensity)", col = cols, border = "grey35", lwd = 1.1, medlwd = 2.4)
  abline(h = 0, lty = 2, col = "grey45")
  abline(v = c(3.5, 6.5), lty = 3, col = "grey80")
}

plot_example_trajectories <- function(trajectories, path) {
  png(path, width = 1200, height = 850, res = 120)
  op <- par(mar = c(6, 6, 4, 2), cex.axis = 0.95, cex.lab = 1.05)
  on.exit({par(op); dev.off()}, add = TRUE)
  plot(NA, xlim = range(trajectories$year), ylim = c(0, 1.05), xlab = "Year", ylab = "Abundance relative to no-disturbance reference", main = "Example population trajectories")
  abline(h = c(1, 0.9, 0.5), lty = c(2, 2, 3), col = c("grey50", "grey70", "grey40"))
  ids <- unique(trajectories$scenario_id)
  for (id in ids) {
    x <- trajectories[trajectories$scenario_id == id, ]
    lt <- ifelse(unique(x$disturbance_type) == "interactive", 1, 3)
    lw <- ifelse(unique(x$disturbance_type) == "interactive", 1.2, 0.8)
    lines(x$year, x$relative_to_reference, lty = lt, lwd = lw)
  }
  legend("bottomleft", legend = c("Reference lines: 1.0, 0.9 and 0.5", "Interactive scenarios shown with darker, thicker lines"), lty = c(3, 1), bty = "n")
}

plot_resilience_outputs <- function(results, figure_dir) {
  plot_final_ratio_by_disturbance(results$metrics, file.path(figure_dir, "final_ratio_by_disturbance.png"))
  plot_interaction_penalty(results$paired_interaction, file.path(figure_dir, "interaction_penalty_final_ratio.png"), "interaction_penalty_final_ratio", "Additional effect of interacting disturbances", "Interactive minus additive final ratio")
  plot_interaction_penalty(results$paired_interaction, file.path(figure_dir, "interaction_penalty_average_loss.png"), "interaction_penalty_average_loss", "Additional average loss from interacting disturbances", "Interactive minus additive average proportional loss")
  plot_box(results$metrics, "average_loss", "life_history", file.path(figure_dir, "average_loss_by_life_history.png"), "Resilience differences among simulated life histories", "Average proportional loss", "Life-history group")
  plot_box(results$species, "baseline_lambda", "life_history", file.path(figure_dir, "baseline_lambda_by_life_history.png"), "Calibrated baseline growth rates", "Baseline population growth rate (lambda)", "Life-history group")
  plot_example_trajectories(results$example_trajectories, file.path(figure_dir, "example_relative_trajectories.png"))
}
