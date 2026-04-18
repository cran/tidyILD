test_that("ild_panel_lag_prepare creates lag columns and check table", {
  d <- ild_simulate(n_id = 6, n_obs_per = 8, seed = 902L)
  d$x <- rnorm(nrow(d))
  d$z <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_panel_lag_prepare(x, c("x", "z"), n = c(1L, 2L), mode = "gap_aware")
  expect_true(all(c("x_lag1", "z_lag2") %in% names(out$data)))
  expect_equal(out$lag_vars, c("x_lag1", "z_lag2"))
  expect_true(nrow(out$check) == 2L)
  expect_true(all(out$check$var %in% out$lag_vars))
})

test_that("ild_panel_lag_prepare recycles n", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 903L)
  d$x <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_panel_lag_prepare(x, c("x", "y"), n = 1L, mode = "index")
  expect_true(all(c("x_lag1", "y_lag1") %in% names(out$data)))
})
