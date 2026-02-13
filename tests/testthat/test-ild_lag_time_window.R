test_that("ild_lag time_window mode with closest_prior", {
  d <- data.frame(
    id = c(1, 1, 1, 1),
    time = as.POSIXct(c(0, 5, 10, 20), origin = "1970-01-01"),
    x = c(10, 20, 30, 40)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, mode = "time_window", window = 15, resolution = "closest_prior")
  # Row 4: time=20, window (5,20); obs at 5,10 in window; closest_prior = 30 (at 10)
  expect_true("x_lag_window" %in% names(out))
  expect_equal(out$x_lag_window[4], 30)
})

test_that("ild_lag time_window mean_in_window", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 10), origin = "1970-01-01"),
    x = c(10, 20, 30)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, mode = "time_window", window = 15, resolution = "mean_in_window")
  # Row 3: time=10, window (-5,10); obs at 0,5 in window; mean = 15
  expect_equal(out$x_lag_window[3], 15)
})

test_that("ild_lag time_window requires window", {
  x <- ild_prepare(ild_simulate(n_id = 2, n_obs_per = 3, seed = 1), id = "id", time = "time")
  expect_error(ild_lag(x, y, mode = "time_window"), "window")
})
