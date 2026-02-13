test_that("ild_spacing_class returns regular-ish for equal intervals", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = as.POSIXct(rep(c(0, 10, 20), 2), origin = "1970-01-01"),
    x = 1:6
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 100)
  expect_equal(ild_spacing_class(x), "regular-ish")
})

test_that("ild_spacing_class returns irregular-ish when cv high", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 100), origin = "1970-01-01"),
    x = 1:3
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  expect_equal(ild_spacing_class(x), "irregular-ish")
})

test_that("ild_spacing_class is overridable via cv_threshold and pct_gap_threshold", {
  d <- data.frame(
    id = c(1, 1, 1),
    time = as.POSIXct(c(0, 5, 100), origin = "1970-01-01"),
    x = 1:3
  )
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  # Relax both so classification becomes regular-ish
  expect_equal(ild_spacing_class(x, cv_threshold = 1, pct_gap_threshold = 100), "regular-ish")
})
