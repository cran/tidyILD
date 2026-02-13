test_that("ild_prepare creates all .ild_* columns", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = 1:4
  )
  x <- ild_prepare(d, id = "id", time = "time")
  expect_true(is_ild(x))
  expect_named(x, c("id", "time", "x", ".ild_id", ".ild_time", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap"))
  expect_equal(x[[".ild_seq"]], c(1, 2, 1, 2))
  expect_equal(x[[".ild_dt"]], c(NA, 10, NA, 10))
})

test_that("ild_prepare sorts by id then time", {
  d <- data.frame(
    id = c(2, 1, 1, 2),
    time = as.POSIXct(c(5, 0, 10, 0), origin = "1970-01-01"),
    x = 1:4
  )
  x <- ild_prepare(d, id = "id", time = "time")
  expect_equal(x[[".ild_id"]], c(1, 1, 2, 2))
  expect_equal(x[[".ild_time_num"]], c(0, 10, 0, 5))
})

test_that("ild_prepare gap_threshold sets .ild_gap", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 100), origin = "1970-01-01"),
    x = 1:3
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  expect_equal(x[[".ild_gap"]], c(NA, FALSE, TRUE))
})

test_that("ild_prepare duplicate_handling first keeps first row", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 0, 10), origin = "1970-01-01"),
    x = c(10, 20, 30)
  )
  x <- ild_prepare(d, id = "id", time = "time", duplicate_handling = "first")
  expect_equal(nrow(x), 2)
  expect_equal(x$x, c(10, 30))
})

test_that("ild_prepare duplicate_handling last keeps last row", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 0, 10), origin = "1970-01-01"),
    x = c(10, 20, 30)
  )
  x <- ild_prepare(d, id = "id", time = "time", duplicate_handling = "last")
  expect_equal(nrow(x), 2)
  expect_equal(x$x, c(20, 30))
})

test_that("ild_prepare duplicate_handling error stops on duplicates", {
  d <- data.frame(
    id = c(1, 1),
    time = as.POSIXct(c(0, 0), origin = "1970-01-01"),
    x = 1:2
  )
  expect_error(ild_prepare(d, id = "id", time = "time", duplicate_handling = "error"), "Duplicate")
})

test_that("ild_prepare with one observation per person", {
  d <- data.frame(id = 1:3, time = as.POSIXct(0:2, origin = "1970-01-01"), x = 1:3)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_equal(x[[".ild_seq"]], c(1, 1, 1))
  expect_true(all(is.na(x[[".ild_dt"]])))
  expect_true(all(is.na(x[[".ild_gap"]])))
})

test_that("ild_prepare spacing is descriptive list", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = 1:4
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 5)
  spacing <- attr(x, "ild_spacing")
  expect_type(spacing, "list")
  expect_true("median_dt" %in% names(spacing))
  expect_true("iqr_dt" %in% names(spacing))
  expect_true("pct_gap" %in% names(spacing))
  expect_true("by_id" %in% names(spacing))
  expect_s3_class(spacing$by_id, "tbl_df")
  expect_named(spacing$by_id, c("id", "median_dt", "iqr_dt", "n_intervals", "pct_gap"))
})

test_that("ild_prepare spacing by_id has one row per person and correct stats", {
  # id 1: times 0, 10, 20 -> intervals 10, 10; median=10, iqr=0. id 2: times 0, 5, 15 -> intervals 5, 10; median=7.5.
  # gap_threshold=8: id1 intervals 10,10 both >8 so pct_gap=100; id2 intervals 5,10 so one >8, pct_gap=50.
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = as.POSIXct(c(0, 10, 20, 0, 5, 15), origin = "1970-01-01"),
    x = 1:6
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 8)
  by_id <- attr(x, "ild_spacing")$by_id
  expect_equal(nrow(by_id), 2)
  by_id <- by_id[order(by_id$id), ]
  expect_equal(by_id$id, c(1, 2))
  expect_equal(by_id$median_dt, c(10, 7.5))
  expect_equal(by_id$n_intervals, c(2L, 2L))
  expect_equal(by_id$pct_gap, c(100, 50))
})

test_that("ild_prepare duplicate_handling collapse aggregates with collapse_fn", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 0, 10), origin = "1970-01-01"),
    x = c(10, 20, 30)
  )
  x <- ild_prepare(d, id = "id", time = "time", duplicate_handling = "collapse", collapse_fn = list(x = mean))
  expect_equal(nrow(x), 2)
  expect_equal(x$x[x$.ild_time_num == 0], 15)
  expect_equal(x$x[x$.ild_time_num == 10], 30)
})
