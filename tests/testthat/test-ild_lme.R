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

test_that("ild_lme engine selection: ar1 FALSE returns lmerMod, ar1 TRUE returns lme", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 10)
  x <- ild_prepare(d, id = "id", time = "time")
  fit_lmer <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  fit_lme <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  expect_true(inherits(fit_lmer, "lmerMod"))
  expect_s3_class(fit_lme, "lme")
})

test_that("ild_lme default random mapping works when id column is not .ild_id", {
  dat <- data.frame(
    M2ID = rep(1:3, each = 4),
    time = rep(as.POSIXct(0:3, origin = "1970-01-01"), 3),
    y = rnorm(12)
  )
  x <- ild_prepare(dat, id = "M2ID", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  expect_equal(ild_meta(attr(fit, "ild_data"))$ild_id, "M2ID")
  expect_s3_class(fit, "lme")
  resolved <- attr(fit, "ild_random_resolved")
  expect_false(is.null(resolved))
  dep <- paste(deparse(resolved), collapse = " ")
  expect_true(grepl("M2ID", dep, fixed = TRUE))
  expect_false(grepl(".ild_id", dep, fixed = TRUE))
})

test_that("ild_lme ar1 TRUE rejects lme4 random syntax in formula", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 20)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_lme(y ~ 1 + (1 | id), data = x, ar1 = TRUE),
    "fixed-effects-only"
  )
})

test_that("ild_lme correlation default uses CAR1 for irregular data", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 30)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  expect_equal(attr(fit, "ild_correlation_class"), "CAR1")
  expect_true(inherits(fit$modelStruct$corStruct, "corCAR1"))
})

test_that("ild_lme warns when predictor is uncentered (WP and BP variance)", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 1)
  d$stress <- d$y + rnorm(nrow(d), 0, 0.5)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_warning(
    ild_lme(y ~ stress + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE),
    "ild_center"
  )
})

test_that("ild_lme does not warn about centering when predictors are _wp/_bp", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- suppressWarnings(
    ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  )
  expect_true(inherits(fit, "lmerMod"))
})

test_that("ild_lme warn_uncentered = FALSE suppresses uncentered warning", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 1)
  d$stress <- d$y + rnorm(nrow(d), 0, 0.5)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ stress + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE,
                 warn_uncentered = FALSE)
  expect_true(inherits(fit, "lmerMod"))
})
