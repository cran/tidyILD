test_that("ild_brms attaches ild_data, ild_posterior, ild_provenance", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 99)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 60, warmup = 30,
    refresh = 0, seed = 99, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  expect_true(!is.null(attr(fit, "ild_data", exact = TRUE)))
  ps <- attr(fit, "ild_posterior", exact = TRUE)
  expect_equal(ps$engine, "brms")
  expect_true(!is.null(ps$prior_summary))
  prov <- attr(fit, "ild_provenance", exact = TRUE)
  last <- prov$analysis_steps[[length(prov$analysis_steps)]]
  expect_equal(last$step, "ild_brms")
})

test_that("ild_tidy and ild_augment work for ild_brms fits", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 100)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 50, warmup = 25,
    refresh = 0, seed = 100, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  tt <- ild_tidy(fit)
  expect_true(all(ild_tidy_schema()$required %in% names(tt)))
  expect_true(all(c("rhat", "ess_bulk", "ess_tail") %in% names(tt)))
  aug <- ild_augment(fit)
  expect_true(all(ild_augment_schema()$required %in% names(aug)))
  expect_true(all(c(".fitted_lower", ".fitted_upper") %in% names(aug)))
  expect_equal(nrow(aug), nrow(x))
})

test_that("ild_diagnose.brmsfit returns ild_diagnostics_bundle", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 101)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 50, warmup = 25,
    refresh = 0, seed = 101, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  diag <- ild_diagnose(fit, type = "convergence")
  expect_s3_class(diag, "ild_diagnostics_bundle")
  expect_true(!is.null(diag$fit$convergence$convergence_table))
})

test_that("ild_methods mentions priors and sampler for ild_brms", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 4, n_obs_per = 4, seed = 102)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 40, warmup = 20,
    refresh = 0, seed = 102, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  txt <- ild_methods(fit)
  expect_true(grepl("ild_brms|Bayesian|prior|chains|warmup|adapt_delta", txt, ignore.case = TRUE))
})
