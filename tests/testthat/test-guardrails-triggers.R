# Canonical guardrail rule_id triggers — deterministic fixtures + fit-level unit tests

test_that("evaluate_guardrails_fit triggers GR_SINGULAR_RANDOM_EFFECTS (lmer)", {
  gr <- tidyILD:::evaluate_guardrails_fit(
    structure(list(), class = "lmerMod"),
    list(singular = TRUE),
    engine = "lmer"
  )
  expect_true(any(gr$rule_id == "GR_SINGULAR_RANDOM_EFFECTS"))
})

test_that("evaluate_guardrails_fit triggers GR_POOR_POSTERIOR_MIXING and GR_LOW_ESS (brms)", {
  gr_mix <- tidyILD:::evaluate_guardrails_fit(
    structure(list(), class = "brmsfit"),
    list(max_rhat = 1.08, convergence_table = tibble::tibble(ess_bulk = 200)),
    engine = "brms"
  )
  expect_true(any(gr_mix$rule_id == "GR_POOR_POSTERIOR_MIXING"))

  gr_ess <- tidyILD:::evaluate_guardrails_fit(
    structure(list(), class = "brmsfit"),
    list(
      max_rhat = 1.01,
      convergence_table = tibble::tibble(ess_bulk = c(150, 50))
    ),
    engine = "brms"
  )
  expect_true(any(gr_ess$rule_id == "GR_LOW_ESS"))
})

test_that("evaluate_guardrails_fit/contextual trigger ctsem guardrails", {
  gr_fit <- tidyILD:::evaluate_guardrails_fit(
    structure(list(), class = "ild_fit_ctsem"),
    list(
      convergence = list(converged = FALSE),
      drift_abs_max = 6
    ),
    engine = "ctsem"
  )
  expect_true(any(gr_fit$rule_id == "GR_CTSEM_NONCONVERGENCE"))
  expect_true(any(gr_fit$rule_id == "GR_CTSEM_UNSTABLE_DYNAMICS"))

  d <- ild_simulate(n_id = 1, n_obs_per = 12, seed = 7110)
  x <- ild_prepare(d, id = "id", time = "time")
  b <- ild_diagnostics_bundle(
    fit = list(state_dimension = 2),
    data = list(pct_gap = NA_real_),
    design = list(),
    residual = list(),
    predictive = list(),
    missingness = list(),
    causal = list(),
    meta = list(),
    warnings = tibble::tibble(),
    guardrails = tidyILD:::guardrails_empty_tibble(),
    summary_text = character()
  )
  gr_ctx <- tidyILD:::evaluate_guardrails_contextual(
    structure(list(), class = "ild_fit_ctsem"),
    x,
    b,
    engine = "ctsem"
  )
  expect_true(any(gr_ctx$rule_id == "GR_CTSEM_SHORT_SERIES_FOR_COMPLEX_DYNAMICS"))
})

test_that("ild_diagnose triggers GR_MIXED_PREDICTOR_NOT_DECOMPOSED", {
  d <- ild_simulate(n_id = 8, n_obs_per = 5, seed = 7001)
  d$stress <- as.numeric(scale(rnorm(nrow(d)))) +
    rep(as.numeric(scale(rnorm(8))), each = 5L) * 0.3
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ stress + y_bp + y_wp + (1 | id), data = x, ar1 = FALSE,
    warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  expect_true(any(b$guardrails$rule_id == "GR_MIXED_PREDICTOR_NOT_DECOMPOSED"))
})

test_that("ild_diagnose triggers GR_IRREGULAR_SPACING_NO_RESID_COR and GR_HIGH_TIMING_GAP_RATE", {
  d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 7002)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 0)
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  expect_true(any(b$guardrails$rule_id == "GR_IRREGULAR_SPACING_NO_RESID_COR"))
  expect_true(any(b$guardrails$rule_id == "GR_HIGH_TIMING_GAP_RATE"))
})

test_that("ild_diagnose triggers GR_IPW_WEIGHTS_UNSTABLE", {
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 7003)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  w <- rep(c(1, 60), length.out = nrow(x))
  x$.ipw <- w
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  expect_true(any(b$guardrails$rule_id == "GR_IPW_WEIGHTS_UNSTABLE"))
})

test_that("evaluate_guardrails_contextual triggers GR_MSM_COMPONENT_WEIGHTS_UNSTABLE", {
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 7003)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  w <- rep(c(1, 60), length.out = nrow(x))
  x$.ipw_treat <- w
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  gr <- tidyILD:::evaluate_guardrails_contextual(fit, x, ild_diagnostics_bundle(), engine = "lmer")
  expect_true(any(gr$rule_id == "GR_MSM_COMPONENT_WEIGHTS_UNSTABLE"))
})

test_that("evaluate_guardrails_contextual triggers GR_DROPOUT_LATE_CONCENTRATION", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 7004)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x$y[x$.ild_seq > 3] <- NA
  xc <- x[stats::complete.cases(x$y), , drop = FALSE]
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = xc, ar1 = FALSE, warn_no_ar1 = FALSE)
  expect_true(tidyILD:::guardrail_dropout_late_heuristic(x, "y"))
  gr <- tidyILD:::evaluate_guardrails_contextual(fit, x, ild_diagnostics_bundle(), engine = "lmer")
  expect_true(any(gr$rule_id == "GR_DROPOUT_LATE_CONCENTRATION"))
})

test_that("ild_guardrails_summary and print bundle include guardrail summary", {
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 7005)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 0)
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  gs <- tidyILD:::ild_guardrails_summary(b)
  expect_true(gs$n >= 1L)
  expect_true(nzchar(gs$narrative))
  pr <- utils::capture.output(print(b))
  expect_true(any(grepl("Summary:", pr, fixed = TRUE)))
  expect_true(any(grepl("guardrails:", pr, fixed = TRUE)))
  expect_true(any(grepl("key rule_id", pr, fixed = TRUE)))
})

test_that("ild_methods appends guardrails when bundle has rows", {
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 7006)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 0)
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  m <- ild_methods(fit, bundle = b)
  expect_true(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
})

test_that("ild_report includes diagnostics_summary guardrails fields when triggered", {
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 7007)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 0)
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  r <- ild_report(fit)
  expect_true(!is.null(r$diagnostics_summary$guardrails_narrative))
  expect_true(!is.null(r$diagnostics_summary$guardrails))
  expect_true(!is.null(r$methods_with_guardrails))
  expect_true(nchar(r$diagnostics_summary$guardrails_narrative) > 0)
})
