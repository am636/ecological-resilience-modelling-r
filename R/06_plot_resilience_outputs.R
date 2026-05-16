# Plotting functions

scenario_axis <- function(labels, label_cex = 0.78, label_offset = 0.085, xlab_line = 5.8) {
  pretty_labels <- gsub("_", "\n", labels)
  positions <- seq_along(labels)

  axis(1, at = positions, labels = FALSE, tick = FALSE)

  usr <- par("usr")
  text(
    x = positions,
    y = usr[3] - label_offset * diff(usr[3:4]),
    labels = pretty_labels,
    xpd = NA,
    cex = label_cex
  )

  mtext(
    "Scenario (D = drought probability, L = land-use intensity)",
    side = 1,
    line = xlab_line,
    cex = 0.9
  )

  abline(v = c(3.5, 6.5), lty = 3, col = "grey82")
}

figure_theme <- function(mar = c(6, 5, 3, 1.2), mfrow = c(1, 1)) {
  par(
    mfrow = mfrow,
    mar = mar,
    mgp = c(2.7, 0.8, 0),
    tcl = -0.25,
    cex.axis = 0.9,
    cex.lab = 1.0,
    cex.main = 1.05,
    family = "sans",
    bty = "l"
  )
}

scenario_colours <- function(labels) {
  cols <- rep(c("#D8E4F0", "#CADCC3", "#E7D7C9"), each = 3)
  names(cols) <- labels
  cols
}

life_history_colours <- function() {
  c(
    fast = "#D8C3B5",
    intermediate = "#C7D4E5",
    slow = "#C9DAC3"
  )
}

plot_life_history_box <- function(data, response, path, title, ylab) {
  levels_lh <- c("fast", "intermediate", "slow")
  data$life_history <- factor(data$life_history, levels = levels_lh)

  png(path, width = 950, height = 720, res = 120)
  op <- par(no.readonly = TRUE)
  on.exit({par(op); dev.off()}, add = TRUE)

  figure_theme(mar = c(5.6, 4.8, 3.0, 1.2))
  boxplot(
    data[[response]] ~ data$life_history,
    ylab = ylab,
    xlab = "Life-history group",
    main = title,
    col = life_history_colours()[levels_lh],
    border = "#4D4D4D",
    lwd = 1.1,
    staplewex = 0.5,
    outcex = 0.6,
    frame.plot = FALSE
  )
}

plot_final_ratio_by_disturbance <- function(metrics, path) {
  labels <- unique(metrics$scenario_label)
  fill <- scenario_colours(labels)

  png(path, width = 1700, height = 820, res = 130)
  op <- par(no.readonly = TRUE)
  on.exit({par(op); dev.off()}, add = TRUE)

  figure_theme(mar = c(8.2, 4.8, 3.0, 1.2), mfrow = c(1, 2))

  for (dist_type in c("additive", "interactive")) {
    x <- metrics[metrics$disturbance_type == dist_type, ]
    x$scenario_label <- factor(x$scenario_label, levels = labels)

    boxplot(
      final_ratio ~ scenario_label,
      data = x,
      ylim = c(0, 1.05),
      xaxt = "n",
      xlab = "",
      ylab = "Final abundance relative to no-disturbance reference",
      main = paste("Disturbance type:", dist_type),
      col = fill[labels],
      border = "#4D4D4D",
      lwd = 1.1,
      staplewex = 0.5,
      outcex = 0.6,
      frame.plot = FALSE
    )

    scenario_axis(labels, label_cex = 0.74, label_offset = 0.085, xlab_line = 5.8)
    abline(h = 1, lty = 2, col = "#6E6E6E")
  }
}

plot_interaction_penalty <- function(paired, path, response, title, ylab) {
  labels <- unique(paired$scenario_label)
  paired$scenario_label <- factor(paired$scenario_label, levels = labels)
  fill <- scenario_colours(labels)

  y <- paired[[response]]
  yr <- range(y, 0, na.rm = TRUE)
  pad <- 0.04 * diff(yr)
  if (!is.finite(pad) || pad == 0) pad <- 0.01

  png(path, width = 1200, height = 780, res = 130)
  op <- par(no.readonly = TRUE)
  on.exit({par(op); dev.off()}, add = TRUE)

  figure_theme(mar = c(8.0, 4.8, 3.0, 1.2))
  boxplot(
    paired[[response]] ~ paired$scenario_label,
    ylim = c(yr[1] - pad, yr[2] + pad),
    xaxt = "n",
    xlab = "",
    ylab = ylab,
    main = title,
    col = fill[labels],
    border = "#4D4D4D",
    lwd = 1.1,
    staplewex = 0.5,
    outcex = 0.6,
    frame.plot = FALSE
  )

  scenario_axis(labels)
  abline(h = 0, lty = 2, col = "#6E6E6E")
}

plot_example_trajectories <- function(trajectories, path) {
  png(path, width = 1100, height = 760, res = 120)
  op <- par(no.readonly = TRUE)
  on.exit({par(op); dev.off()}, add = TRUE)

  figure_theme(mar = c(5.2, 4.8, 3.0, 1.2))

  plot(
    NA,
    xlim = range(trajectories$year),
    ylim = c(0, 1.05),
    xlab = "Year",
    ylab = "Abundance relative to no-disturbance reference",
    main = "Example population trajectories",
    frame.plot = FALSE
  )

  abline(h = c(0.5, 0.9, 1.0), lty = c(3, 2, 2), col = c("#7A7A7A", "#B0B0B0", "#7A7A7A"))

  trajectory_ids <- unique(
    paste(
      trajectories$species_id,
      trajectories$scenario_label,
      trajectories$disturbance_type,
      sep = "__"
    )
  )

  line_types <- rep(c(1, 2, 3, 4, 5, 6), length.out = length(trajectory_ids))

  for (k in seq_along(trajectory_ids)) {
    id <- trajectory_ids[k]
    parts <- strsplit(id, "__", fixed = TRUE)[[1]]
    x <- trajectories[
      trajectories$species_id == parts[1] &
        trajectories$scenario_label == parts[2] &
        trajectories$disturbance_type == parts[3],
    ]

    lines(
      x$year,
      x$relative_to_reference,
      col = ifelse(parts[3] == "interactive", "#303030", "#7F7F7F"),
      lty = line_types[k],
      lwd = ifelse(parts[3] == "interactive", 1.8, 1.1)
    )
  }

  legend(
    "bottomleft",
    legend = c("Reference lines: 1.0, 0.9 and 0.5", "Interactive scenarios shown with darker, thicker lines"),
    lty = c(2, 1),
    lwd = c(1, 1.8),
    col = c("#7A7A7A", "#303030"),
    bty = "n",
    cex = 0.88
  )
}

plot_resilience_outputs <- function(results, figure_dir) {
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  plot_final_ratio_by_disturbance(
    results$metrics,
    file.path(figure_dir, "final_ratio_by_disturbance.png")
  )

  plot_interaction_penalty(
    results$paired_interaction,
    file.path(figure_dir, "interaction_penalty_final_ratio.png"),
    "interaction_penalty_final_ratio",
    "Additional effect of interacting disturbances",
    "Interactive minus additive final ratio"
  )

  plot_interaction_penalty(
    results$paired_interaction,
    file.path(figure_dir, "interaction_penalty_average_loss.png"),
    "interaction_penalty_average_loss",
    "Additional average loss from interacting disturbances",
    "Interactive minus additive average proportional loss"
  )

  plot_life_history_box(
    results$metrics,
    "average_loss",
    file.path(figure_dir, "average_loss_by_life_history.png"),
    "Resilience differences among simulated life histories",
    "Average proportional loss"
  )

  plot_life_history_box(
    results$species,
    "baseline_lambda",
    file.path(figure_dir, "baseline_lambda_by_life_history.png"),
    "Calibrated baseline growth rates",
    "Baseline population growth rate (lambda)"
  )

  plot_example_trajectories(
    results$example_trajectories,
    file.path(figure_dir, "example_relative_trajectories.png")
  )

  invisible(figure_dir)
}