# Contract invariant tests (ILD object)

test_that("contract: within each .ild_id, .ild_time_num is non-decreasing", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2),
    time = as.POSIXct(c(0, 5, 10, 0, 5), origin = "1970-01-01"),
    x = 1:5
  )
  x <- ild_prepare(d, id = "id", time = "time")
  for (uid in unique(x[[".ild_id"]])) {
    tnum <- x[[".ild_time_num"]][x[[".ild_id"]] == uid]
    expect_true(all(diff(tnum) >= 0, na.rm = TRUE))
  }
})

test_that("contract: .ild_seq is exactly 1:n_i per id", {
  d <- data.frame(id = c(1, 1, 1, 2, 2), time = as.POSIXct(0:4, origin = "1970-01-01"), x = 1:5)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_equal(x[[".ild_seq"]], c(1, 2, 3, 1, 2))
})

test_that("contract: .ild_dt equals .ild_time_num - lag(.ild_time_num) within id", {
  d <- data.frame(id = c(1, 1, 1, 2, 2), time = as.POSIXct(c(0, 3, 7, 0, 4), origin = "1970-01-01"), x = 1:5)
  x <- ild_prepare(d, id = "id", time = "time")
  dt_expected <- c(NA, 3, 4, NA, 4)
  expect_equal(x[[".ild_dt"]], dt_expected)
})

test_that("contract: .ild_gap is TRUE where .ild_dt > gap_threshold, NA for first row per id", {
  d <- data.frame(id = c(1, 1, 1), time = as.POSIXct(c(0, 5, 100), origin = "1970-01-01"), x = 1:3)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  expect_equal(x[[".ild_gap"]], c(NA, FALSE, TRUE))
})

test_that("contract: .ild_time_num consistent with .ild_time for POSIXct", {
  d <- data.frame(id = 1, time = as.POSIXct(100, origin = "1970-01-01"), x = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_equal(x[[".ild_time_num"]], 100)
})
