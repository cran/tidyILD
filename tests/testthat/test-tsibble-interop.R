test_that("ild_prepare omit id/time captures tsibble meta and spacing bridge", {
  skip_if_not_installed("tsibble")
  suppressPackageStartupMessages(library(tsibble))
  t <- yearmonth(c(201901, 201902, 201903, 201901, 201902, 201903))
  d <- tsibble(
    id = rep(c("a", "b"), each = 3),
    t = t,
    y = rnorm(6),
    key = id,
    index = t
  )
  x <- ild_prepare(d)
  tm <- ild_tsibble_meta(x)
  expect_type(tm, "list")
  expect_identical(tm$source_class, "tbl_ts")
  expect_identical(tm$key_vars, "id")
  expect_identical(tm$index_var, "t")
  expect_true(is.logical(tm$is_regular) && !is.na(tm$is_regular))
  sp <- attr(x, "ild_spacing", exact = TRUE)
  expect_true(!is.null(sp$tsibble))
  expect_identical(sp$tsibble$index_var, "t")
})

test_that("ild_prepare explicit id/time on tbl_ts still captures meta", {
  skip_if_not_installed("tsibble")
  suppressPackageStartupMessages(library(tsibble))
  t <- yearmonth(c(201901, 201902, 201903))
  d <- tsibble(id = rep(1, 3), t = t, y = 1:3, key = id, index = t)
  x <- ild_prepare(d, id = "id", time = "t")
  tm <- ild_tsibble_meta(x)
  expect_identical(tm$source_class, "tbl_ts")
})

test_that("ild_as_tsibble round-trip preserves interval for unchanged regular series", {
  skip_if_not_installed("tsibble")
  suppressPackageStartupMessages(library(tsibble))
  t <- yearmonth(c(201901, 201902, 201903))
  d <- tsibble(id = rep(1, 3), t = t, y = 1:3, key = id, index = t)
  iv0 <- tsibble::interval(d)
  x <- ild_prepare(d)
  d2 <- ild_as_tsibble(x)
  expect_s3_class(d2, "tbl_ts")
  expect_true(identical(tsibble::interval(d2), iv0))
})

test_that("non-tsibble ild_prepare has NULL ild_tsibble_meta", {
  d <- data.frame(id = c(1, 1), time = 1:2, y = 1:2)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_null(ild_tsibble_meta(x))
})
