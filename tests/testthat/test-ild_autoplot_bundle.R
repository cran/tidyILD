test_that("ild_diagnose attaches ild_fit and ild_data on bundle", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 201)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_identical(attr(b, "ild_fit", exact = TRUE), fit)
  expect_identical(attr(b, "ild_data", exact = TRUE), x)
})

test_that("ild_autoplot bundle: residual legacy list when type is NULL", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 202)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  pl <- ild_autoplot(b)
  expect_type(pl, "list")
  expect_true(length(pl) >= 1L)
})

test_that("ild_autoplot bundle: section residual types return ggplot", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 203)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_s3_class(ild_autoplot(b, section = "residual", type = "acf"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "qq"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "fitted"), "ggplot")
})

test_that("ild_autoplot bundle: fit, data, design sections", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 204)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "data", type = "missingness"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "design", type = "coverage"), "ggplot")
})

test_that("ild_autoplot bundle: causal weights errors without .ipw", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 205)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_error(ild_autoplot(b, section = "causal", type = "weights"), "IPW")
})

test_that("ild_autoplot bundle: causal weights with .ipw", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 206)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x$.ipw <- rep(1, nrow(x))
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_s3_class(ild_autoplot(b, section = "causal", type = "weights"), "ggplot")
})

test_that("ild_autoplot bundle: brms residual acf, convergence, ppc", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 207)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 50, warmup = 25,
    refresh = 0, seed = 207, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  b <- ild_diagnose(fit, type = "all")
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  p <- ild_autoplot(b, section = "predictive", type = "ppc", ndraws = 20)
  expect_s3_class(p, "ggplot")
})

test_that("ild_autoplot bundle: frequentist predictive ppc errors", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 208)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_error(ild_autoplot(b, section = "predictive", type = "ppc"), "brmsfit")
})
