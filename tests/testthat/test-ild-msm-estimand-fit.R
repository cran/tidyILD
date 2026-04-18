test_that("ild_msm_estimand builds backward-compatible static ATE object", {
  e <- ild_msm_estimand(treatment = "trt")
  expect_s3_class(e, "ild_msm_estimand")
  expect_equal(e$type, "ate")
  expect_equal(e$regime, "static")
  expect_true(is.list(e$contrast))
  expect_identical(e$target_time, "all")
})

test_that("ild_msm_estimand accepts structured regime specification", {
  e <- ild_msm_estimand(
    type = "att",
    treatment = "trt",
    regime = list(class = "dynamic", rule = "always_treat"),
    target_time = c(3, 5),
    contrast = list(label = "treated_vs_control", treated = 1, control = 0)
  )
  expect_equal(e$type, "att")
  expect_equal(e$regime, "dynamic")
  expect_true(is.list(e$regime_spec))
  expect_equal(e$regime_spec$class, "dynamic")
  expect_equal(e$target_time, c(3, 5))
})

test_that("ild_msm_fit returns structured object with provenance", {
  d <- msm_fixture_data(seed = 2201, n_id = 12, n_obs_per = 6)
  e <- ild_msm_estimand(treatment = "trt")
  fit_obj <- ild_msm_fit(
    estimand = e,
    data = d,
    outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
    history = ~ stress_lag1 + trt_lag1,
    predictors_censor = "stress",
    inference = "none",
    warn_no_ar1 = FALSE,
    warn_uncentered = FALSE
  )
  expect_s3_class(fit_obj, "ild_msm_fit")
  expect_true(inherits(fit_obj$fit, "lmerMod"))
  expect_true(is_ild(fit_obj$weights_data))
  expect_true(".ipw" %in% names(fit_obj$weights_data))
  expect_true(is.list(fit_obj$inference))
  expect_equal(fit_obj$inference$status, "ok")
  pv <- attr(fit_obj, "ild_provenance", exact = TRUE)
  expect_true(is.list(pv))
  expect_equal(pv$object_type, "ild_msm_fit")
})

test_that("ild_msm_fit supports robust and bootstrap inference modes", {
  d <- msm_fixture_data(seed = 2202, n_id = 10, n_obs_per = 5)
  e <- ild_msm_estimand(treatment = "trt")

  r1 <- ild_msm_fit(
    estimand = e,
    data = d,
    outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
    history = ~ stress_lag1 + trt_lag1,
    inference = "robust",
    robust_type = "CR2",
    warn_no_ar1 = FALSE,
    warn_uncentered = FALSE
  )
  expect_true(r1$inference$method %in% c("robust", "model_fallback"))
  expect_true(r1$inference$status %in% c("ok", "degraded"))
  expect_s3_class(r1$inference$summary, "tbl_df")
  expect_true(all(c("term", "terms", "estimate", "std_error", "conf_low", "conf_high", "interval_type", "method") %in% names(r1$inference$summary)))

  r2 <- ild_msm_fit(
    estimand = e,
    data = d,
    outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
    history = ~ stress_lag1 + trt_lag1,
    inference = "bootstrap",
    n_boot = 6L,
    seed = 1,
    warn_no_ar1 = FALSE,
    warn_uncentered = FALSE
  )
  expect_equal(r2$inference$method, "bootstrap")
  expect_equal(r2$inference$status, "ok")
  expect_s3_class(r2$inference$raw$bootstrap$result, "ild_msm_bootstrap")
})

test_that("dynamic regime is scaffolded with explicit degraded status", {
  d <- msm_fixture_data(seed = 2203, n_id = 10, n_obs_per = 5)
  e_dyn <- ild_msm_estimand(
    treatment = "trt",
    regime = "dynamic",
    dynamic_rule = "always_treat",
    target_time = "final",
    contrast = list(label = "always_vs_never")
  )
  expect_warning(
    fit_dyn <- ild_msm_fit(
      estimand = e_dyn,
      data = d,
      outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
      history = ~ stress_lag1 + trt_lag1,
      inference = "none",
      strict_inference = FALSE,
      warn_no_ar1 = FALSE,
      warn_uncentered = FALSE
    ),
    "Dynamic regime assignment"
  )
  expect_equal(fit_dyn$regime_status$status, "degraded")
  expect_true(".msm_regime_assignment" %in% names(fit_dyn$weights_data))
  expect_error(
    ild_msm_fit(
      estimand = e_dyn,
      data = d,
      outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
      history = ~ stress_lag1 + trt_lag1,
      inference = "none",
      strict_inference = TRUE,
      warn_no_ar1 = FALSE,
      warn_uncentered = FALSE
    )
  )
})

test_that("ild_msm_contrast_over_time returns time-indexed contrasts", {
  d <- msm_fixture_data(seed = 2204, n_id = 10, n_obs_per = 5)
  e <- ild_msm_estimand(treatment = "trt")
  fit_obj <- ild_msm_fit(
    estimand = e,
    data = d,
    outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
    history = ~ stress_lag1 + trt_lag1,
    inference = "none",
    warn_no_ar1 = FALSE,
    warn_uncentered = FALSE
  )
  ct <- ild_msm_contrast_over_time(fit_obj, time_var = ".ild_seq")
  expect_s3_class(ct, "tbl_df")
  expect_true(all(c("term", "estimate", "std_error", "conf_low", "conf_high", "interval_type", "method") %in% names(ct)))
  expect_true(nrow(ct) >= 1L)
  dg <- ild_msm_diagnose(fit_obj, balance = FALSE)
  expect_s3_class(dg, "ild_diagnostics_bundle")
})
