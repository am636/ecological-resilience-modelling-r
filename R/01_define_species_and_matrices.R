# ============================================================
# Species definitions and stage-structured matrices
# ============================================================

matrix_lambda <- function(A) {
  values <- eigen(A, only.values = TRUE)$values
  max(Re(values))
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
      c("seedling", "juvenile", "adult"),
      c("seedling", "juvenile", "adult")
    )
  )
}

calibrate_fecundity <- function(pars, target_lambda) {
  objective <- function(fecundity) {
    trial <- pars
    trial$fecundity <- fecundity
    matrix_lambda(build_matrix(trial)) - target_lambda
  }

  uniroot(objective, lower = 0.01, upper = 20)$root
}

simulate_species_traits <- function(n_per_group = 8, seed = 42) {
  set.seed(seed)

  groups <- c("fast", "intermediate", "slow")
  species <- vector("list", length(groups) * n_per_group)
  index <- 1

  for (group in groups) {
    for (i in seq_len(n_per_group)) {
      if (group == "fast") {
        pars <- list(
          seedling_stasis = runif(1, 0.10, 0.25),
          seedling_to_juvenile = runif(1, 0.30, 0.45),
          juvenile_stasis = runif(1, 0.15, 0.30),
          juvenile_to_adult = runif(1, 0.25, 0.40),
          adult_survival = runif(1, 0.50, 0.65),
          fecundity = NA_real_,
          disturbance_tolerance = runif(1, 0.45, 0.70)
        )
        target_lambda <- runif(1, 1.020, 1.040)
      } else if (group == "intermediate") {
        pars <- list(
          seedling_stasis = runif(1, 0.15, 0.30),
          seedling_to_juvenile = runif(1, 0.20, 0.35),
          juvenile_stasis = runif(1, 0.25, 0.45),
          juvenile_to_adult = runif(1, 0.15, 0.30),
          adult_survival = runif(1, 0.68, 0.82),
          fecundity = NA_real_,
          disturbance_tolerance = runif(1, 0.55, 0.80)
        )
        target_lambda <- runif(1, 1.015, 1.030)
      } else {
        pars <- list(
          seedling_stasis = runif(1, 0.20, 0.35),
          seedling_to_juvenile = runif(1, 0.10, 0.22),
          juvenile_stasis = runif(1, 0.40, 0.60),
          juvenile_to_adult = runif(1, 0.05, 0.16),
          adult_survival = runif(1, 0.84, 0.94),
          fecundity = NA_real_,
          disturbance_tolerance = runif(1, 0.65, 0.90)
        )
        target_lambda <- runif(1, 1.005, 1.018)
      }

      pars$fecundity <- calibrate_fecundity(pars, target_lambda)
      lambda <- matrix_lambda(build_matrix(pars))

      species[[index]] <- data.frame(
        species_id = sprintf("sp_%02d", index),
        life_history = group,
        seedling_stasis = pars$seedling_stasis,
        seedling_to_juvenile = pars$seedling_to_juvenile,
        juvenile_stasis = pars$juvenile_stasis,
        juvenile_to_adult = pars$juvenile_to_adult,
        adult_survival = pars$adult_survival,
        fecundity = pars$fecundity,
        disturbance_tolerance = pars$disturbance_tolerance,
        baseline_lambda = lambda,
        stringsAsFactors = FALSE
      )

      index <- index + 1
    }
  }

  do.call(rbind, species)
}

species_row_to_pars <- function(row) {
  list(
    seedling_stasis = row$seedling_stasis,
    seedling_to_juvenile = row$seedling_to_juvenile,
    juvenile_stasis = row$juvenile_stasis,
    juvenile_to_adult = row$juvenile_to_adult,
    adult_survival = row$adult_survival,
    fecundity = row$fecundity,
    disturbance_tolerance = row$disturbance_tolerance
  )
}
