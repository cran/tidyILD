# MSM balance, ESS, overlap

test_that("ild_ipw_ess equals nrow for constant unit weights", {
  set.seed(400)
  d <- ild_simulate(n_id = 2, n_obs_per = 2, seed = 400)
  x <- ild_prepare(d, id = "id", time = "time")
  x$.ipw <- rep(1, nrow(x))
  expect_equal(ild_ipw_ess(x, ".ipw"), nrow(x))
})

test_that("ild_ipw_ess pooled and by occasion", {
  set.seed(401)
  d <- ild_simulate(n_id = 4, n_obs_per = 3, seed = 401)
  x <- ild_prepare(d, id = "id", time = "time")
  x$.ipw <- runif(nrow(x), 0.8, 1.2)
  e0 <- ild_ipw_ess(x, ".ipw", by_occasion = FALSE)
  expect_true(is.numeric(e0) && is.finite(e0))
  e1 <- ild_ipw_ess(x, ".ipw", by_occasion = TRUE)
  expect_s3_class(e1, "tbl_df")
  expect_true("ess" %in% names(e1))
})

test_that("ild_msm_balance pooled returns one row per covariate", {
  set.seed(402)
  d <- ild_simulate(n_id = 20, n_obs_per = 6, seed = 402)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x$.ipw_treat <- runif(nrow(x), 0.85, 1.15)
  tab <- ild_msm_balance(x, treatment = "trt", covariates = "stress", weights_col = ".ipw_treat")
  expect_equal(nrow(tab), 1L)
  expect_true(all(c("stratum", "covariate", "smd", "mean_treated", "mean_control") %in% names(tab)))
  expect_equal(tab$stratum[1], "pooled")
})

test_that("ild_msm_overlap_plot pooled uses ild_iptw_fit", {
  skip_if_not_installed("ggplot2")
  set.seed(403)
  d <- ild_simulate(n_id = 15, n_obs_per = 5, seed = 403)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
  g <- ild_msm_overlap_plot(x, treatment = "trt", source = "pooled")
  expect_s3_class(g, "ggplot")
})

test_that("ild_diagnose balance adds causal$balance when requested", {
  set.seed(404)
  d <- ild_simulate(n_id = 12, n_obs_per = 5, seed = 404)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
  fit <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = x,
    ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  b <- ild_diagnose(fit, data = x, balance = TRUE, balance_treatment = "trt",
    balance_covariates = "stress", balance_weights_col = ".ipw_treat")
  expect_true(!is.null(b$causal$balance))
  expect_s3_class(b$causal$balance$table, "tbl_df")
  expect_true(!is.null(b$causal$balance$ess))
})

test_that("ild_autoplot causal overlap requires treatment", {
  skip_if_not_installed("ggplot2")
  set.seed(405)
  d <- ild_simulate(n_id = 10, n_obs_per = 5, seed = 405)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
  fit <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = x,
    ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  b <- ild_diagnose(fit, data = x)
  expect_error(
    ild_autoplot(b, section = "causal", type = "overlap"),
    "treatment"
  )
  p <- ild_autoplot(b, section = "causal", type = "overlap", treatment = "trt")
  expect_s3_class(p, "ggplot")
})
