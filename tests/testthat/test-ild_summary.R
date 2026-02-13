test_that("ild_summary returns expected structure", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = 1:4
  )
  # gap_threshold 50 so intervals (10, 10) are not gaps
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 50)
  s <- ild_summary(x)
  expect_named(s, c("n_units", "n_obs", "time_range", "spacing", "n_gaps", "pct_gap"))
  expect_equal(s$n_units, 2)
  expect_equal(s$n_obs, 4)
  expect_equal(s$time_range, c(0, 10))
  expect_equal(s$n_gaps, 0)
})

test_that("ild_summary counts gaps", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 100), origin = "1970-01-01"),
    x = 1:3
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  s <- ild_summary(x)
  expect_equal(s$n_gaps, 1)
})
