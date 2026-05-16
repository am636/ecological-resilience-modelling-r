# Disturbance scenarios and matrix adjustment

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
  grid$scenario_label <- sprintf("D%.2f_L%.2f", grid$drought_probability, grid$land_use_intensity)
  grid$scenario_id <- paste(grid$scenario_label, grid$disturbance_type, sep = "_")
  grid
}

make_seed <- function(species_index, drought_probability, land_use_intensity) {
  100000L + as.integer(species_index) * 1000L + as.integer(round(drought_probability * 1000)) * 10L + as.integer(round(land_use_intensity * 1000))
}

make_drought_sequence <- function(n_years, drought_probability, seed) {
  set.seed(seed)
  rbinom(n_years, size = 1, prob = drought_probability)
}

apply_disturbance_to_matrix <- function(A, pars, drought_event, land_use_intensity, interaction_strength) {
  drought_pressure <- as.numeric(drought_event)
  land_pressure <- land_use_intensity
  tolerance <- pars$disturbance_tolerance
  interaction_pressure <- drought_pressure * land_pressure * interaction_strength
  total_pressure <- drought_pressure + land_pressure + interaction_pressure

  survival_multiplier <- clamp(1 - total_pressure * (0.12 + 0.14 * (1 - tolerance)), 0.60, 1)
  growth_multiplier <- clamp(1 - total_pressure * (0.16 + 0.10 * (1 - tolerance)), 0.55, 1)
  fecundity_multiplier <- clamp(1 - total_pressure * (0.28 + 0.18 * (1 - tolerance)), 0.35, 1)

  A["seedling_next", "seedling"] <- A["seedling_next", "seedling"] * survival_multiplier
  A["juvenile_next", "juvenile"] <- A["juvenile_next", "juvenile"] * survival_multiplier
  A["adult_next", "adult"] <- A["adult_next", "adult"] * survival_multiplier
  A["juvenile_next", "seedling"] <- A["juvenile_next", "seedling"] * growth_multiplier
  A["adult_next", "juvenile"] <- A["adult_next", "juvenile"] * growth_multiplier
  A["seedling_next", "adult"] <- A["seedling_next", "adult"] * fecundity_multiplier
  A
}
