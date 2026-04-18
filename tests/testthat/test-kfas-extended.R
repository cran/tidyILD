# KFAS: contract depth, simulation recovery, missingness-as-documented, guardrails,
# and skipped placeholders for future state_spec DGPs.

# Helpers: helper-kfas-fixtures.R

test_that("KFAS contract depth: tidy/augment/diagnose structure", {
  skip_if_not_installed("KFAS")
  dgp <- kfas_dgp_local_level(
    n = 80L,
    Q = 1,
    H = 1,
    seed = KFAS_TEST_SEED_CONTRACT
  )
  fit <- suppressWarnings(
    ild_kfas(dgp$data, outcome = "y", state_spec = "local_level", time_units = "steps")
  )

  tt <- ild_tidy(fit)
  sch <- ild_tidy_schema()
  expect_true(all(sch$required %in% names(tt)))
  expect_true(all(tt$engine == "KFAS"))
  expect_true(all(tt$model_class == "ild_fit_kfas"))
  expect_true(all(tt$component %in% c("variance", "auxiliary")))
  expect_true(all(tt$interval_type == "none"))

  aug <- ild_augment(fit)
  a_sch <- ild_augment_schema()
  expect_true(all(a_sch$required %in% names(aug)))
  expect_equal(nrow(aug), nrow(attr(fit, "ild_data")))
  expect_true(is.numeric(aug$.fitted))
  expect_true(is.numeric(aug$.resid))
  expect_true(abs(stats::cor(aug[[".outcome"]], aug[[".fitted"]], use = "complete.obs")) > 0.2)

  b <- ild_diagnose(fit)
  expect_kfas_bundle_core(b)
  expect_true(is.character(b$summary_text) || is.null(b$summary_text))
})

test_that("KFAS recovery: local level Q/H and smoothed state track simulation", {
  skip_if_not_installed("KFAS")
  n <- 500L
  Q <- 1
  H <- 1
  dgp <- kfas_dgp_local_level(n, Q = Q, H = H, seed = KFAS_TEST_SEED_RECOVERY)
  fit <- suppressWarnings(
    ild_kfas(dgp$data, outcome = "y", state_spec = "local_level", time_units = "steps")
  )

  tt <- ild_tidy(fit)
  q_hat <- tt$estimate[tt$term == "level_variance"]
  h_hat <- tt$estimate[tt$term == "observation_variance"]
  expect_length(q_hat, 1L)
  expect_length(h_hat, 1L)
  expect_true(is.finite(q_hat) && is.finite(h_hat) && q_hat > 0 && h_hat > 0)
  expect_true(max(q_hat / Q, Q / q_hat) < 5)
  expect_true(max(h_hat / H, H / h_hat) < 5)

  ah <- fit$kfs$alphahat
  expect_true(is.matrix(ah))
  sm <- as.numeric(ah[, 1L])
  expect_equal(length(sm), nrow(attr(fit, "ild_data")))
  expect_true(stats::cor(dgp$true_level, sm) > 0.85)

  fh <- 5L
  fit_f <- suppressWarnings(
    ild_kfas(
      dgp$data,
      outcome = "y",
      state_spec = "local_level",
      time_units = "steps",
      forecast_horizon = fh
    )
  )
  # Forecast path is ggplot + optional KFAS ahead mean (API varies by KFAS version)
  expect_s3_class(ild_plot_forecast(fit_f), "ggplot")
})

test_that("KFAS missingness: NA outcomes omitted with warning; preprocessing counts", {
  skip_if_not_installed("KFAS")
  x <- kfas_fixture_segmented_na_outcome()
  expect_true(sum(is.na(x$y)) >= 1L)
  expect_warning(
    fit <- ild_kfas(x, outcome = "y", state_spec = "local_level", time_units = "steps"),
    "Removing rows with NA"
  )
  expect_equal(fit$preprocessing$n_rows_input, nrow(x))
  expect_equal(fit$preprocessing$n_outcome_na, sum(is.na(x$y)))
  expect_equal(fit$preprocessing$n_rows_used, sum(!is.na(x$y)))
  expect_true(fit$preprocessing$n_na_segments >= 3L)
})

test_that("KFAS guardrail: high irregularity for discrete-time indexing", {
  skip_if_not_installed("KFAS")
  x <- kfas_fixture_irregular_intervals(n = 55L)
  sc <- ild_spacing_class(x)
  expect_true(sc == "irregular-ish" || !is.na(sc))
  fit <- suppressWarnings(
    ild_kfas(x, outcome = "y", state_spec = "local_level", time_units = "steps", irregular_time = TRUE)
  )
  b <- ild_diagnose(fit)
  expect_true(any(b$guardrails$rule_id == "GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME"))
})

test_that("KFAS guardrail: many missing outcome segments", {
  skip_if_not_installed("KFAS")
  x <- kfas_fixture_segmented_na_outcome()
  fit <- suppressWarnings(
    ild_kfas(x, outcome = "y", state_spec = "local_level", time_units = "steps")
  )
  b <- ild_diagnose(fit)
  expect_true(any(b$guardrails$rule_id == "GR_KFAS_MANY_MISSING_OUTCOME_SEGMENTS"))
})

# --- Future state_spec DGP recovery (unimplemented wrappers): placeholders ---

test_that("KFAS recovery DGP: local trend (skipped until state_spec implemented)", {
  skip_if_not_installed("KFAS")
  skip("local_trend not implemented in ild_kfas(); see inst/dev/KFAS_V1_BACKEND.md")
  n <- 300L
  Q <- c(0.1, 0.1)
  H <- 0.5
  expect_true(n > 0 && length(Q) == 2L && H > 0)
})

test_that("KFAS recovery DGP: AR(1) latent (skipped until state_spec implemented)", {
  skip_if_not_installed("KFAS")
  skip("ar1_state not implemented in ild_kfas(); see inst/dev/KFAS_V1_BACKEND.md")
  phi <- 0.7
  Q <- 0.2
  H <- 0.4
  expect_true(abs(phi) < 1 && Q > 0 && H > 0)
})

test_that("KFAS recovery DGP: regression + local level (skipped until state_spec implemented)", {
  skip_if_not_installed("KFAS")
  skip("regression_local_level not implemented in ild_kfas(); see inst/dev/KFAS_V1_BACKEND.md")
  n <- 250L
  beta <- 0.3
  expect_true(n > 5L && is.finite(beta))
})

test_that("KFAS guardrail: state dimension high for n (skipped until multi-state local_level or larger state_spec)", {
  skip_if_not_installed("KFAS")
  skip("GR_KFAS_STATE_DIMENSION_HIGH_FOR_N requires m_state >= 2; v1 local_level is scalar state")
  expect_true(TRUE)
})
