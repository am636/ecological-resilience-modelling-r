# Stage-structured species and matrix setup

spectral_radius <- function(A) {
  as.numeric(max(Mod(eigen(A, only.values = TRUE)$values)))
}

clamp <- function(x, lower, upper) {
  pmax(lower, pmin(upper, x))
}

safe_ratio <- function(numerator, denominator) {
  out <- rep(NA_real_, length(numerator))
  ok <- is.finite(denominator) & denominator > 0
  out[ok] <- numerator[ok] / denominator[ok]
  out
}

build_matrix <- function(pars) {
  matrix(
    c(
      pars$seedling_stasis, 0, pars$fecundity,
      pars$seedling_to_juvenile, pars$juvenile_stasis, 0,
      0, pars$juvenile_to_adult, pars$adult_survival
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("seedling_next", "juvenile_next", "adult_next"),
      c("seedling", "juvenile", "adult")
    )
  )
}

lambda_for_fecundity <- function(pars, fecundity) {
  pars$fecundity <- fecundity
  spectral_radius(build_matrix(pars))
}

calibrate_fecundity <- function(pars, target_lambda) {
  lower <- 0
  upper <- 1
  while (lambda_for_fecundity(pars, upper) < target_lambda) {
    upper <- upper * 2
    if (upper > 100) stop("Could not bracket target lambda.", call. = FALSE)
  }
  for (i in seq_len(80)) {
    mid <- (lower + upper) / 2
    if (lambda_for_fecundity(pars, mid) < target_lambda) lower <- mid else upper <- mid
  }
  (lower + upper) / 2
}

simulate_species_table <- function(n_per_group = 8, seed = 42) {
  set.seed(seed)
  groups <- c("fast", "intermediate", "slow")
  out <- list()
  k <- 1L
  for (life_history in groups) {
    for (i in seq_len(n_per_group)) {
      if (life_history == "fast") {
        target_lambda <- max(1.002, rnorm(1, 1.030, 0.004))
        pars <- list(seedling_stasis = runif(1, 0.10, 0.18), seedling_to_juvenile = runif(1, 0.25, 0.40), juvenile_stasis = runif(1, 0.30, 0.45), juvenile_to_adult = runif(1, 0.25, 0.40), adult_survival = runif(1, 0.60, 0.76))
        disturbance_tolerance <- runif(1, 0.30, 0.70)
      } else if (life_history == "intermediate") {
        target_lambda <- max(1.002, rnorm(1, 1.020, 0.003))
        pars <- list(seedling_stasis = runif(1, 0.12, 0.22), seedling_to_juvenile = runif(1, 0.18, 0.30), juvenile_stasis = runif(1, 0.40, 0.58), juvenile_to_adult = runif(1, 0.15, 0.28), adult_survival = runif(1, 0.72, 0.86))
        disturbance_tolerance <- runif(1, 0.35, 0.75)
      } else {
        target_lambda <- max(1.002, rnorm(1, 1.010, 0.0025))
        pars <- list(seedling_stasis = runif(1, 0.15, 0.25), seedling_to_juvenile = runif(1, 0.10, 0.22), juvenile_stasis = runif(1, 0.55, 0.72), juvenile_to_adult = runif(1, 0.08, 0.18), adult_survival = runif(1, 0.84, 0.94))
        disturbance_tolerance <- runif(1, 0.40, 0.80)
      }
      pars$fecundity <- calibrate_fecundity(pars, target_lambda)
      out[[length(out) + 1L]] <- data.frame(species_index = k, species_id = sprintf("sp_%03d", k), life_history = life_history, target_lambda = target_lambda, baseline_lambda = spectral_radius(build_matrix(pars)), disturbance_tolerance = disturbance_tolerance, seedling_stasis = pars$seedling_stasis, seedling_to_juvenile = pars$seedling_to_juvenile, juvenile_stasis = pars$juvenile_stasis, juvenile_to_adult = pars$juvenile_to_adult, adult_survival = pars$adult_survival, fecundity = pars$fecundity, stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  ans <- do.call(rbind, out)
  ans$life_history <- factor(ans$life_history, levels = groups)
  ans
}

species_row_to_pars <- function(row) {
  list(seedling_stasis = row$seedling_stasis, seedling_to_juvenile = row$seedling_to_juvenile, juvenile_stasis = row$juvenile_stasis, juvenile_to_adult = row$juvenile_to_adult, adult_survival = row$adult_survival, fecundity = row$fecundity, disturbance_tolerance = row$disturbance_tolerance)
}
