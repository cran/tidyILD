# IPTW / IPCW / joint MSM weights

test_that("ild_iptw_weights + ild_ipcw_weights + ild_joint_msm_weights pipeline", {
  set.seed(901)
  d <- ild_simulate(n_id = 25, n_obs_per = 8, seed = 901)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
  x <- ild_ipcw_weights(x, predictors = "stress")
  expect_true(all(is.finite(x$.ipw_treat)))
  expect_true(all(is.finite(x$.ipw_censor)))
  expect_true(all(x$.ipw_treat > 0))
  expect_true(all(x$.ipw_censor > 0))
  xj <- ild_joint_msm_weights(x, stabilize = "none", trim = numeric(0))
  expect_equal(xj$.ipw, xj$.ipw_treat * xj$.ipw_censor)
  xm <- ild_joint_msm_weights(x, stabilize = "mean1", trim = numeric(0))
  expect_equal(mean(xm$.ipw, na.rm = TRUE), 1, tolerance = 1e-4)
  wprod <- xm$.ipw_treat * xm$.ipw_censor
  expect_equal(xm$.ipw, wprod / mean(wprod, na.rm = TRUE), tolerance = 1e-3)
})

test_that("ild_ipw_refit and ild_diagnose work with joint MSM .ipw", {
  set.seed(902)
  d <- ild_simulate(n_id = 20, n_obs_per = 7, seed = 902)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x <- ild_iptw_weights(x, "trt", "stress")
  x <- ild_ipcw_weights(x, "stress")
  x <- ild_joint_msm_weights(x)
  fit <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = x,
    ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  fitw <- ild_ipw_refit(fit, data = x)
  expect_true(inherits(fitw, "lmerMod"))
  b <- ild_diagnose(fitw, data = x, type = "qq")
  expect_true(!is.null(b$causal))
  expect_true(!is.null(b$causal$weight_summary))
  expect_true(!is.null(b$causal$weight_summary_treat))
  expect_true(!is.null(b$causal$weight_summary_censor))
})

test_that("ild_iptw_weights rejects non-binary treatment", {
  set.seed(903)
  d <- ild_simulate(n_id = 10, n_obs_per = 5, seed = 903)
  d$stress <- rnorm(nrow(d))
  d$trt3 <- rep(1:3, length.out = nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_iptw_weights(x, "trt3", "stress"),
    "binary treatment"
  )
})
