test_that("ild_msm_simulate_scenario returns ILD with treatment and truth", {
  d <- ild_msm_simulate_scenario(
    n_id = 12,
    n_obs_per = 6,
    true_ate = 0.4,
    positivity_stress = 1.2,
    seed = 2301
  )
  expect_true(is_ild(d))
  expect_true(all(c("stress", "trt") %in% names(d)))
  expect_equal(attr(d, "true_ate", exact = TRUE), 0.4)
})

test_that("ild_msm_recovery fast tier returns summary and per-sim metrics", {
  set.seed(2302)
  out <- ild_msm_recovery(
    n_sim = 3L,
    n_id = 20L,
    n_obs_per = 6L,
    true_ate = 0.5,
    n_boot = 8L,
    inference = "bootstrap",
    seed = 99L,
    censoring = TRUE
  )
  expect_s3_class(out, "ild_msm_recovery")
  expect_s3_class(out$summary, "tbl_df")
  expect_s3_class(out$summary_by_scenario, "tbl_df")
  expect_s3_class(out$sim_results, "tbl_df")
  expect_true(all(c("bias", "rmse", "coverage", "ess_mean", "ess_p50") %in% names(out$summary)))
  expect_true(all(c("failure_rate", "inference_degraded_rate", "inference_unsupported_rate") %in% names(out$summary_by_scenario)))
  expect_true(nrow(out$sim_results) == 3L)
  pv <- attr(out, "ild_provenance", exact = TRUE)
  expect_true(is.list(pv))
  expect_equal(pv$object_type, "ild_msm_recovery")
})

test_that("ild_msm_recovery scenario grid works (extended tier)", {
  skip_on_cran()
  grid <- msm_recovery_scenario_grid_fixture()
  out <- ild_msm_recovery(
    n_sim = 2L,
    n_id = 20L,
    n_obs_per = 6L,
    true_ate = 0.5,
    inference = "none",
    scenario_grid = grid,
    seed = 2310
  )
  expect_equal(nrow(out$summary_by_scenario), nrow(grid))
  expect_true(all(c("scenario_id", "propensity_extreme_rate", "coverage_calibration_gap", "inference_method_modal") %in% names(out$summary_by_scenario)))
  expect_equal(nrow(out$sim_results), 2L * nrow(grid))
})
