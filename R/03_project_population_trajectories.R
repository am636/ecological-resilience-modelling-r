# Population projection and simulation runner

project_population <- function(pars, drought_sequence, land_use_intensity = 0, interaction_strength = 0, initial_population = c(seedling = 120, juvenile = 80, adult = 50)) {
  n_years <- length(drought_sequence)
  stage_abundance <- matrix(NA_real_, nrow = n_years + 1, ncol = 3)
  colnames(stage_abundance) <- names(initial_population)
  stage_abundance[1, ] <- initial_population
  total_abundance <- numeric(n_years + 1)
  total_abundance[1] <- sum(initial_population)
  annual_lambda <- rep(NA_real_, n_years)
  baseline_A <- build_matrix(pars)

  for (year in seq_len(n_years)) {
    A_year <- apply_disturbance_to_matrix(baseline_A, pars, drought_sequence[year], land_use_intensity, interaction_strength)
    next_state <- as.numeric(A_year %*% stage_abundance[year, ])
    stage_abundance[year + 1, ] <- next_state
    total_abundance[year + 1] <- sum(next_state)
    annual_lambda[year] <- safe_ratio(total_abundance[year + 1], total_abundance[year])
  }

  data.frame(year = 0:n_years, seedling = stage_abundance[, 1], juvenile = stage_abundance[, 2], adult = stage_abundance[, 3], total_abundance = total_abundance, annual_lambda = c(NA_real_, annual_lambda), stringsAsFactors = FALSE)
}
