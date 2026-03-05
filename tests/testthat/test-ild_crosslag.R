test_that("ild_crosslag returns fit, lag_term, lag_check, data", {
  d <- ild_simulate(n_id = 8, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_crosslag(x, y, y, lag = 1, ar1 = FALSE, warn_no_ar1 = FALSE)
  expect_named(out, c("fit", "lag_term", "lag_check", "data"))
  expect_true(inherits(out$fit, "lmerMod"))
  expect_s3_class(out$lag_term, "tbl_df")
  expect_true("y_lag1" %in% out$lag_term$term)
  expect_s3_class(out$lag_check, "tbl_df")
  expect_equal(out$lag_check$var, "y_lag1")
  expect_true(is_ild(out$data))
  expect_true("y_lag1" %in% names(out$data))
})

test_that("ild_crosslag with include_diagnostics adds diagnostics slot", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 2)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_crosslag(x, "y", "y", lag = 1, mode = "index", include_diagnostics = TRUE,
                      ar1 = FALSE)
  expect_true("diagnostics" %in% names(out))
  if (!is.null(out$diagnostics)) expect_s3_class(out$diagnostics, "ild_diagnostics")
})

test_that("ild_crosslag errors on missing outcome or predictor", {
  x <- ild_prepare(ild_simulate(n_id = 3, n_obs_per = 4, seed = 1), id = "id", time = "time")
  expect_error(ild_crosslag(x, "nonexistent", "y", lag = 1, warn_no_ar1 = FALSE), "not found")
  expect_error(ild_crosslag(x, "y", "nonexistent", lag = 1, warn_no_ar1 = FALSE), "not found")
})
