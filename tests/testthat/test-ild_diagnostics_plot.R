test_that("ild_diagnostics returns expected structure", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  diag <- ild_diagnostics(fit, data = x)
  expect_named(diag, c("residuals", "fitted", "id", "time", "seq", "ar1_param", "by_id", "acf", "plot"))
  expect_equal(length(diag$residuals), nrow(x))
  expect_true(is.list(diag$plot))
  expect_true("qq" %in% names(diag$plot))
})

test_that("ild_plot trajectory runs without error", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_plot(x, type = "trajectory", var = "y")
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot gaps runs without error", {
  d <- ild_simulate(n_id = 2, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_plot(x, type = "gaps")
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot fitted runs with ild_lme object", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  p <- ild_plot(fit, type = "fitted")
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot residual_acf runs with ild_lme object", {
  d <- ild_simulate(n_id = 3, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  p <- ild_plot(fit, type = "residual_acf")
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot missingness runs and returns ggplot", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  d$y[2] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_plot(x, type = "missingness", var = "y")
  expect_s3_class(p, "ggplot")
})
