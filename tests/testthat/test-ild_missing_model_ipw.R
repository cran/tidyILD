test_that("ild_missing_model returns fit and tidy when there is missingness", {
  set.seed(1)
  d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 1)
  d$stress <- rnorm(nrow(d))
  d$mood <- d$y
  d$mood[sample(nrow(d), 15)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  mm <- ild_missing_model(x, "mood", "stress")
  expect_true(is.list(mm))
  expect_named(mm, c("fit", "tidy", "outcome", "predictors", "p_missing", "message"))
  expect_s3_class(mm$fit, "glm")
  expect_s3_class(mm$tidy, "tbl_df")
  expect_equal(mm$outcome, "mood")
  expect_equal(mm$predictors, "stress")
  expect_equal(length(mm$p_missing), nrow(x))
})

test_that("ild_missing_model returns early when no missingness", {
  set.seed(1)
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 1)
  d$stress <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  mm <- ild_missing_model(x, "y", "stress")
  expect_null(mm$fit)
  expect_true(grepl("No missingness", mm$message))
})

test_that("ild_ipw_weights returns data with .ipw and preserves ILD attrs", {
  set.seed(1)
  d <- ild_simulate(n_id = 10, n_obs_per = 6, seed = 1)
  d$stress <- rnorm(nrow(d))
  d$mood <- d$y
  d$mood[sample(nrow(d), 10)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  mm <- ild_missing_model(x, "mood", "stress")
  xw <- ild_ipw_weights(x, mm, stabilize = TRUE, trim = c(0.01, 0.99))
  expect_true(".ipw" %in% names(xw))
  expect_true(all(is.finite(xw$.ipw)))
  expect_equal(attr(xw, "ild_id"), attr(x, "ild_id"))
})

test_that("ild_ipw_refit runs and returns lmerMod with ild_data", {
  set.seed(1)
  d <- ild_simulate(n_id = 8, n_obs_per = 6, seed = 1)
  d$stress <- rnorm(nrow(d))
  d$mood <- d$y
  d$mood[sample(nrow(d), 8)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, mood)
  mm <- ild_missing_model(x, "mood", "stress")
  xw <- ild_ipw_weights(x, mm, stabilize = TRUE)
  fit0 <- ild_lme(mood ~ mood_bp + mood_wp + stress + (1 | id), data = xw,
    ar1 = FALSE, warn_no_ar1 = FALSE)
  fitw <- ild_ipw_refit(fit0, data = xw)
  expect_true(inherits(fitw, "lmerMod"))
  expect_false(is.null(attr(fitw, "ild_data")))
  tbl <- tidy_ild_model(fitw)
  expect_s3_class(tbl, "tbl_df")
  expect_named(tbl, c("term", "estimate", "std_error", "ci_low", "ci_high", "p_value"))
})

test_that("ild_ipw_refit errors when ar1 = TRUE", {
  expect_error(ild_ipw_refit(structure(list(), class = "lmerMod"), data = mtcars, ar1 = TRUE), "ar1 = TRUE")
})
