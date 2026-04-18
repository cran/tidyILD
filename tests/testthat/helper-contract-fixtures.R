# Canonical contract-regression fixtures — deterministic seeds, semantic expectations.
# Loaded automatically by testthat.

#' @keywords internal
contract_tidy_required <- function() {
  ild_tidy_schema()$required
}

#' @keywords internal
contract_augment_required <- function() {
  ild_augment_schema()$required
}

#' Assert ild_tidy / ild_augment column contracts for a frequentist ild_lme fit
#' @keywords internal
contract_assert_tidy_augment_lmer <- function(fit) {
  td <- tidy_ild_model(fit, object = FALSE)
  req <- contract_tidy_required()
  expect_true(all(req %in% names(td)))
  ag <- augment_ild_model(fit)
  expect_true(all(contract_augment_required() %in% names(ag)))
}

#' Core bundle sections populated by ild_diagnose(lmer)
#' @keywords internal
contract_assert_bundle_dense_lmer <- function(b) {
  expect_true(!is.null(b$meta))
  expect_true(!is.null(b$data))
  expect_true(!is.null(b$data$cohort))
  expect_true(!is.null(b$data$obs_per_id))
  expect_true(!is.null(b$design))
  expect_true(!is.null(b$design$flags))
  expect_true(!is.null(b$fit))
  expect_true(!is.null(b$residual))
  expect_true(!is.null(b$predictive))
  expect_true(!is.null(b$missingness))
  expect_true(!is.null(attr(b, "ild_fit", exact = TRUE)))
  expect_true(!is.null(attr(b, "ild_data", exact = TRUE)))
}

#' Frequentist autoplot entry points return ggplot
#' @keywords internal
contract_assert_autoplot_frequentist_core <- function(b) {
  expect_s3_class(ild_autoplot(b, section = "residual", type = "acf"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "qq"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "fitted"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "data", type = "missingness"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "design", type = "coverage"), "ggplot")
}

## --- Fixtures (fixed seeds) -------------------------------------------------

#' Regular-ish spacing, WP/BP decomposition, no intentional guardrails
fixture_contract_regular_decomposed <- function() {
  d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 9101L)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  list(
    name = "regular_decomposed",
    data = x,
    fit = fit,
    bundle = b,
    spacing_class = ild_spacing_class(x),
    expect_rule_ids = character(0),
    expect_methods_guardrail = FALSE
  )
}

#' Irregular spacing, no AR1 — irregular + high gap guardrails
fixture_contract_irregular_no_ar1 <- function() {
  d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 9102L)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 0)
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  list(
    name = "irregular_no_ar1",
    data = x,
    fit = fit,
    bundle = b,
    spacing_class = ild_spacing_class(x),
    expect_rule_ids = c("GR_IRREGULAR_SPACING_NO_RESID_COR", "GR_HIGH_TIMING_GAP_RATE"),
    expect_methods_guardrail = TRUE
  )
}

#' Merge singular-fit guardrail row into a real diagnosed bundle (contract for GR_SINGULAR)
fixture_contract_singular_guardrail <- function() {
  f <- fixture_contract_regular_decomposed()
  gr <- tidyILD:::evaluate_guardrails_fit(
    structure(list(), class = "lmerMod"),
    list(singular = TRUE),
    engine = "lmer"
  )
  f$bundle$guardrails <- dplyr::bind_rows(f$bundle$guardrails, gr)
  f$name <- "singular_lmer_guardrail"
  f$expect_rule_ids <- "GR_SINGULAR_RANDOM_EFFECTS"
  f$expect_methods_guardrail <- TRUE
  f
}

#' Unstable IPW weights
fixture_contract_ipw_unstable <- function() {
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 9103L)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x$.ipw <- rep(c(1, 60), length.out = nrow(x))
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  list(
    name = "ipw_unstable",
    data = x,
    fit = fit,
    bundle = b,
    spacing_class = ild_spacing_class(x),
    expect_rule_ids = "GR_IPW_WEIGHTS_UNSTABLE",
    expect_methods_guardrail = TRUE
  )
}

#' Late dropout: fit on complete cases; full data has NA pattern; merge contextual dropout rows
fixture_contract_dropout_late <- function() {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 9104L)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x$y[x$.ild_seq > 3] <- NA
  xc <- x[stats::complete.cases(x$y), , drop = FALSE]
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = xc, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = xc, type = "qq")
  gr <- tidyILD:::evaluate_guardrails_contextual(fit, x, b, engine = "lmer")
  gr <- gr[gr$rule_id == "GR_DROPOUT_LATE_CONCENTRATION", , drop = FALSE]
  b$guardrails <- dplyr::bind_rows(b$guardrails, gr)
  list(
    name = "dropout_late",
    data_full = x,
    data_fit = xc,
    fit = fit,
    bundle = b,
    expect_rule_ids = "GR_DROPOUT_LATE_CONCENTRATION",
    expect_methods_guardrail = TRUE
  )
}

#' Brms: diagnosed bundle + merged poor-mixing / low-ESS guardrails (no MCMC dependency)
fixture_contract_brms_poor_mixing_merged <- function() {
  skip_if_not_installed("brms")
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 9105L)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1,
    iter = 80,
    warmup = 40,
    refresh = 0,
    seed = 9105L,
    silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  b <- ild_diagnose(fit, type = "all")
  gr_mix <- tidyILD:::evaluate_guardrails_fit(
    fit,
    list(max_rhat = 1.09, convergence_table = tibble::tibble(ess_bulk = 200, term = "b_Intercept")),
    engine = "brms"
  )
  gr_ess <- tidyILD:::evaluate_guardrails_fit(
    fit,
    list(
      max_rhat = 1.01,
      convergence_table = tibble::tibble(ess_bulk = c(40, 30), term = c("a", "b"))
    ),
    engine = "brms"
  )
  b$guardrails <- dplyr::bind_rows(b$guardrails, gr_mix, gr_ess)
  list(
    name = "brms_poor_mixing_merged",
    data = x,
    fit = fit,
    bundle = b,
    expect_rule_ids = c("GR_POOR_POSTERIOR_MIXING", "GR_LOW_ESS"),
    expect_methods_guardrail = TRUE
  )
}
