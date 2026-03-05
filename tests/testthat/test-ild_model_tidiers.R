test_that("augment_ild_model returns tibble with required columns", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  aug <- augment_ild_model(fit)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(c(".ild_id", ".ild_time", "y", ".fitted", ".resid") %in% names(aug)))
  expect_equal(nrow(aug), nrow(x))
  expect_equal(aug$.resid, stats::residuals(fit))
  expect_equal(aug$.fitted, stats::fitted(fit))
})

test_that("augment_ild_model errors when ild_data missing", {
  fit <- lme4::lmer(y ~ 1 + (1 | id), data = data.frame(id = rep(1:2, each = 3), y = rnorm(6)))
  expect_error(augment_ild_model(fit), "refit using ild_lme")
})

test_that("tidy_ild_model returns consistent columns for lmer", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  tt <- tidy_ild_model(fit)
  expect_s3_class(tt, "tbl_df")
  expect_true(all(c("term", "estimate", "std_error", "ci_low", "ci_high", "p_value") %in% names(tt)))
  expect_equal(tt$term, "(Intercept)")
})

test_that("tidy_ild_model returns consistent columns for lme", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  tt <- tidy_ild_model(fit)
  expect_s3_class(tt, "tbl_df")
  expect_true(all(c("term", "estimate", "std_error", "ci_low", "ci_high", "p_value") %in% names(tt)))
  expect_true(is.numeric(tt$p_value))
})
