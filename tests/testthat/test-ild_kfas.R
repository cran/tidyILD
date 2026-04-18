test_that("ild_kfas local_level fits and matches tidy/augment/diagnose contracts", {
  skip_if_not_installed("KFAS")
  d <- ild_simulate(n_id = 1, n_obs_per = 40, seed = 9001)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- suppressWarnings(
    ild_kfas(x, outcome = "y", state_spec = "local_level", time_units = "sim_steps")
  )
  expect_s3_class(fit, "ild_fit_kfas")
  expect_true(!is.null(attr(fit, "ild_data")))
  expect_true(!is.null(attr(fit, "ild_provenance")))

  tt <- ild_tidy(fit)
  expect_true(all(ild_tidy_schema()$required %in% names(tt)))
  expect_true(all(tt$engine == "KFAS"))

  aug <- ild_augment(fit)
  expect_true(all(ild_augment_schema()$required %in% names(aug)))
  expect_true(nrow(aug) == nrow(attr(fit, "ild_data")))

  st <- ild_tidy_states(fit)
  expect_equal(nrow(st), nrow(attr(fit, "ild_data")))

  b <- ild_diagnose(fit)
  expect_s3_class(b, "ild_diagnostics_bundle")
  expect_no_error(validate_ild_diagnostics_bundle(b))
  expect_true(!is.null(b$residual$standardized_prediction_errors))
  expect_true(b$meta$engine == "KFAS")
  expect_true("log_likelihood" %in% names(b$fit))

  p <- suppressWarnings(ild_autoplot(fit, type = "states"))
  expect_s3_class(p, "ggplot")

  expect_s3_class(ild_autoplot(b, section = "residual", type = "acf"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "qq"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "fitted"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "predictive", type = "forecast"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "predictive", type = "errors"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "data", type = "missingness"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "design", type = "coverage"), "ggplot")

  expect_s3_class(ild_plot_states(fit), "ggplot")
  expect_s3_class(ild_plot_filtered_vs_smoothed(fit), "ggplot")
  expect_s3_class(ild_plot_forecast(fit), "ggplot")

  mt <- ild_methods(fit)
  expect_true(grepl("ild_kfas", mt, fixed = TRUE) || grepl("state-space", mt, fixed = TRUE))
})

test_that("KFAS-specific guardrails can trigger", {
  skip_if_not_installed("KFAS")
  d <- ild_simulate(n_id = 1, n_obs_per = 10, seed = 9003)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- suppressWarnings(
    ild_kfas(
      x,
      outcome = "y",
      state_spec = "local_level",
      time_units = "steps",
      fit_context = "independent_series_per_id"
    )
  )
  b <- ild_diagnose(fit)
  expect_true(any(b$guardrails$rule_id == "GR_KFAS_SHORT_SERIES_FOR_STATE_SPACE"))
  expect_true(any(b$guardrails$rule_id == "GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY"))
})

test_that("ild_kfas rejects multiple subjects in v1", {
  skip_if_not_installed("KFAS")
  d <- ild_simulate(n_id = 3, n_obs_per = 10, seed = 9002)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_kfas(x, outcome = "y", time_units = "d"),
    "one subject"
  )
})
