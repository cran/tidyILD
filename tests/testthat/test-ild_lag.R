test_that("ild_lag index mode equals previous row within person", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 20, 0, 10), origin = "1970-01-01"),
    x = c(10, 20, 30, 40, 50)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, n = 1, mode = "index")
  expect_equal(out$x_lag1, c(NA, 10, 20, NA, 40))
})

test_that("ild_lag no cross-person contamination", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = c(100, 200, 300, 400)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, mode = "index")
  expect_true(is.na(out$x_lag1[1]))
  expect_equal(out$x_lag1[2], 100)
  expect_true(is.na(out$x_lag1[3]))
  expect_equal(out$x_lag1[4], 300)
})

test_that("ild_lag gap_aware sets NA when interval exceeds max_gap", {
  d <- data.frame(
    id = c(1, 1, 1, 1),
    time = as.POSIXct(c(0, 5, 10, 100), origin = "1970-01-01"),
    x = c(1, 2, 3, 4)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, n = 1, mode = "gap_aware", max_gap = 20)
  # .ild_dt: NA, 5, 5, 90. So lag at row 4 would be 3, but dt[4]=90 > 20 -> NA
  expect_equal(out$x_lag1, c(NA, 1, 2, NA))
})

test_that("ild_lag gap_aware keeps value when interval within max_gap", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 10), origin = "1970-01-01"),
    x = c(1, 2, 3)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, n = 1, mode = "gap_aware", max_gap = 20)
  expect_equal(out$x_lag1, c(NA, 1, 2))
})

test_that("ild_lag preserves ILD attributes", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, mode = "index")
  expect_true(is_ild(out))
})

test_that("ild_lag n=2 lags by two rows", {
  d <- data.frame(
    id = c(1, 1, 1, 1),
    time = as.POSIXct(0:3, origin = "1970-01-01"),
    x = c(10, 20, 30, 40)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_lag(x, x, n = 2, mode = "index")
  expect_equal(out$x_lag2, c(NA, NA, 10, 20))
})
