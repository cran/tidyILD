test_that("ild_lme ar1 = FALSE fits with lmer", {
  d <- ild_simulate(n_id = 4, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  expect_true(inherits(fit, "lmerMod"))
  expect_false(attr(fit, "ild_ar1"))
  expect_true(!is.null(attr(fit, "ild_data")))
  expect_equal(length(stats::residuals(fit)), nrow(x))
})

test_that("ild_lme ar1 = TRUE fits with nlme and CAR1", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 2)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE, correlation_class = "CAR1")
  expect_s3_class(fit, "lme")
  expect_true(inherits(fit, "ild_lme"))
  expect_true(attr(fit, "ild_ar1"))
  expect_equal(attr(fit, "ild_correlation_class"), "CAR1")
})

test_that("ild_lme correlation_class override", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, seed = 3)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE, correlation_class = "AR1")
  expect_equal(attr(fit, "ild_correlation_class"), "AR1")
})
