test_that("is_ild returns FALSE for non-data frames", {
  expect_false(is_ild(1))
  expect_false(is_ild("a"))
  expect_false(is_ild(NULL))
})

test_that("is_ild returns FALSE when required columns missing", {
  df <- data.frame(.ild_id = 1, .ild_time = Sys.time(), .ild_time_num = 1)
  expect_false(is_ild(df))
})

test_that("validate_ild errors on non-data frame", {
  expect_error(validate_ild(1), "data frame or tibble")
})

test_that("validate_ild errors when required columns missing", {
  df <- data.frame(a = 1)
  expect_error(validate_ild(df), "missing required columns")
})

test_that("ild_meta returns list with expected names", {
  # Use ild_prepare to get a valid ILD object
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 1, 0, 1), origin = "1970-01-01"),
    x = 1:4
  )
  x <- ild_prepare(d, id = "id", time = "time")
  meta <- ild_meta(x)
  expect_type(meta, "list")
  expect_named(meta, c("ild_id", "ild_time", "ild_gap_threshold", "ild_n_units", "ild_n_obs", "ild_spacing"))
  expect_equal(meta$ild_id, "id")
  expect_equal(meta$ild_time, "time")
  expect_equal(meta$ild_n_units, 2)
  expect_equal(meta$ild_n_obs, 4)
})

test_that("as_ild validates and adds ild_tbl class", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 1, 0, 1), origin = "1970-01-01"),
    x = 1:4
  )
  x <- ild_prepare(d, id = "id", time = "time")
  y <- as_ild(x)
  expect_s3_class(y, "ild_tbl")
})
