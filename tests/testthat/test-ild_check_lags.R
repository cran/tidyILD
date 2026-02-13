test_that("ild_check_lags returns expected columns", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_lag(x, y, mode = "gap_aware", max_gap = 7200)
  out <- ild_check_lags(x, lag_vars = "y_lag1", max_gap = 7200)
  expect_named(out, c("var", "n_valid", "n_invalid", "n_first", "n_total", "pct_valid"))
  expect_equal(out$var, "y_lag1")
  expect_equal(out$n_first, 3)
})

test_that("ild_check_lags with no lag vars returns empty tibble", {
  x <- ild_prepare(ild_simulate(n_id = 2, n_obs_per = 3, seed = 1), id = "id", time = "time")
  out <- ild_check_lags(x, lag_vars = character(0))
  expect_equal(nrow(out), 0)
})
