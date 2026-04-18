test_that("ild_diagnostics returns expected structure", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  diag <- ild_diagnostics(fit, data = x)
  expect_true(all(c("meta", "data", "stats") %in% names(diag)))
  expect_equal(nrow(diag$data$residuals), nrow(x))
  expect_true(all(c(".ild_id", ".ild_time", ".resid", ".fitted") %in% names(diag$data$residuals)))
  expect_true("type" %in% names(diag$meta))
  plots <- plot_ild_diagnostics(diag)
  expect_true(is.list(plots))
  expect_true("qq" %in% names(plots))
})

test_that("ild_plot trajectory runs without error", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_plot(x, type = "trajectory", var = "y")
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot trajectory facet_by runs when column exists", {
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 2)
  d$grp <- rep(c("a", "b"), length.out = nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_plot(x, type = "trajectory", var = "y", facet_by = "grp", max_ids = 4L)
  expect_s3_class(p, "ggplot")
})

test_that("ild_plot facet_by errors when column missing", {
  d <- ild_simulate(n_id = 2, n_obs_per = 4, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_plot(x, type = "trajectory", var = "y", facet_by = "nosuch"),
    "facet_by column"
  )
})

test_that("ild_plot_predicted_trajectory and type predicted_trajectory return ggplot", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 3)
  d$grp <- rep(letters[1:2], length.out = nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  p1 <- ild_plot_predicted_trajectory(fit, time_var = ".ild_seq", max_ids = 4L)
  expect_s3_class(p1, "ggplot")
  p2 <- ild_plot(fit, type = "predicted_trajectory", time_var = ".ild_seq", max_ids = 4L, facet_by = "grp")
  expect_s3_class(p2, "ggplot")
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

test_that("ild_diagnostics type = residual_acf returns ACF; lme includes ar1_param", {
  d <- ild_simulate(n_id = 3, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  diag_lmer <- ild_diagnostics(ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE), data = x, type = "residual_acf")
  expect_true(!is.null(diag_lmer$stats$acf$pooled))
  expect_true("residual_acf" %in% names(plot_ild_diagnostics(diag_lmer)))
  fit_lme <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  diag_lme <- ild_diagnostics(fit_lme, type = "residual_acf")
  expect_true("ar1_param" %in% names(diag_lme$stats))
  expect_true(is.numeric(diag_lme$stats$ar1_param) || is.null(diag_lme$stats$ar1_param))
})

test_that("ild_diagnostics type = residual_time returns residuals vs time and plot", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  diag <- ild_diagnostics(fit, data = x, type = "residual_time")
  expect_true(".fitted" %in% names(diag$data$residuals))
  plots <- plot_ild_diagnostics(diag)
  expect_true("residuals_vs_time" %in% names(plots))
  expect_true("residuals_vs_fitted" %in% names(plots))
})

test_that("ild_diagnostics type = qq returns Q-Q plot", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  diag <- ild_diagnostics(fit, data = x, type = "qq")
  expect_true("qq" %in% names(plot_ild_diagnostics(diag)))
  expect_true(is.null(diag$stats$acf))
})

test_that("ild_diagnostics type = c(residual_acf, qq) returns only those plot elements and acf", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  diag <- ild_diagnostics(fit, data = x, type = c("residual_acf", "qq"))
  expect_true(!is.null(diag$stats$acf))
  plots <- plot_ild_diagnostics(diag)
  expect_true("residual_acf" %in% names(plots))
  expect_true("qq" %in% names(plots))
  expect_false("residuals_vs_fitted" %in% names(plots))
  expect_false("residuals_vs_time" %in% names(plots))
})

test_that("ild_plot type = fitted_vs_actual works like fitted", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  p_fitted <- ild_plot(fit, type = "fitted")
  p_fva <- ild_plot(fit, type = "fitted_vs_actual")
  expect_s3_class(p_fva, "ggplot")
  expect_s3_class(p_fitted, "ggplot")
})

test_that("ild_plot type = c(fitted_vs_actual, residual_acf) returns list of two ggplots", {
  d <- ild_simulate(n_id = 3, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  plist <- ild_plot(fit, type = c("fitted_vs_actual", "residual_acf"))
  expect_true(is.list(plist))
  expect_equal(names(plist), c("fitted_vs_actual", "residual_acf"))
  expect_s3_class(plist$fitted_vs_actual, "ggplot")
  expect_s3_class(plist$residual_acf, "ggplot")
})

test_that("ild_plot fitted and residual_acf work with lme fit (ar1 = TRUE)", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit_lme <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  p_fitted <- ild_plot(fit_lme, type = "fitted")
  p_acf <- ild_plot(fit_lme, type = "residual_acf")
  expect_s3_class(p_fitted, "ggplot")
  expect_s3_class(p_acf, "ggplot")
})
