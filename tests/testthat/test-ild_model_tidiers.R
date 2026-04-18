test_that("augment_ild_model returns ild_augment_schema() columns", {
  req <- ild_augment_schema()$required
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  aug <- augment_ild_model(fit)
  expect_s3_class(aug, "tbl_df")
  expect_true(all(req %in% names(aug)))
  expect_equal(nrow(aug), nrow(x))
  expect_equal(aug$.resid, as.numeric(stats::residuals(fit)))
  expect_equal(aug$.fitted, as.numeric(stats::fitted(fit)))
  expect_equal(aug$.outcome, as.numeric(x$y))
  expect_true(is.numeric(aug$.resid_std))
})

test_that("augment_ild_model errors when ild_data missing", {
  fit <- lme4::lmer(y ~ 1 + (1 | id), data = data.frame(id = rep(1:2, each = 3), y = rnorm(6)))
  expect_error(augment_ild_model(fit), "refit using ild_lme")
})

test_that("tidy_ild_model returns ild_tidy_schema() columns for lmer", {
  req <- ild_tidy_schema()$required
  d <- ild_simulate(n_id = 3, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  tt <- tidy_ild_model(fit)
  expect_s3_class(tt, "tbl_df")
  expect_true(all(req %in% names(tt)))
  expect_equal(tt$term, "(Intercept)")
  expect_true(all(tt$component == "fixed"))
  expect_equal(tt$effect_level[1], "population")
  expect_equal(tt$interval_type[1], "Wald")
  expect_equal(tt$engine[1], "lmer")
  expect_true(grepl("lmerMod", tt$model_class[1]))
})

test_that("tidy_ild_model effect_level uses _wp/_bp suffixes", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 2)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  tt <- tidy_ild_model(fit)
  expect_equal(tt$effect_level[tt$term == "y_wp"], "within")
  expect_equal(tt$effect_level[tt$term == "y_bp"], "between")
})

test_that("tidy_ild_model returns ild_tidy_schema() columns for lme", {
  req <- ild_tidy_schema()$required
  d <- ild_simulate(n_id = 3, n_obs_per = 6, irregular = TRUE, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1, data = x, ar1 = TRUE)
  tt <- tidy_ild_model(fit)
  expect_s3_class(tt, "tbl_df")
  expect_true(all(req %in% names(tt)))
  expect_true(is.numeric(tt$p_value))
  expect_equal(tt$engine[1], "lme")
})
