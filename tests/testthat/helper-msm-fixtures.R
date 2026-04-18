msm_fixture_data <- function(seed = 1L, n_id = 12L, n_obs_per = 6L) {
  set.seed(seed)
  d <- ild_simulate(n_id = n_id, n_obs_per = n_obs_per, seed = seed)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  d <- ild_prepare(d, id = "id", time = "time")
  d <- ild_center(d, y)
  d <- ild_lag(d, stress, mode = "gap_aware", max_gap = Inf)
  d <- ild_lag(d, trt, mode = "gap_aware", max_gap = Inf)
  d
}

msm_recovery_scenario_grid_fixture <- function() {
  tibble::tibble(
    scenario_id = c("baseline", "positivity_stress", "misspecified_treatment"),
    positivity_stress = c(1, 1.8, 1),
    misspec_treatment_model = c(FALSE, FALSE, TRUE)
  )
}
