# ============================================================
# Disturbance scenario definitions
# ============================================================

create_disturbance_scenarios <- function() {
  grid <- expand.grid(
    drought_probability = c(0.10, 0.30, 0.50),
    land_use_intensity = c(0.00, 0.25, 0.50),
    disturbance_type = c("additive", "interactive"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  grid <- grid[order(grid$land_use_intensity, grid$drought_probability, grid$disturbance_type), ]
  rownames(grid) <- NULL

  grid$interaction_strength <- ifelse(grid$disturbance_type == "interactive", 1.35, 0)
  grid$scenario_label <- sprintf(
    "D%.2f_L%.2f",
    grid$drought_probability,
    grid$land_use_intensity
  )
  grid$scenario_id <- sprintf(
    "%s_%s",
    grid$scenario_label,
    grid$disturbance_type
  )

  grid
}

make_drought_sequence <- function(n_years, drought_probability, seed) {
  set.seed(seed)
  rbinom(n_years, size = 1, prob = drought_probability)
}
