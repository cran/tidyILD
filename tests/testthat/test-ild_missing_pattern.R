test_that("ild_missing_pattern returns by_id and overall", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = c(1, NA, 3, 4)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  mp <- ild_missing_pattern(x, vars = "x")
  expect_named(mp, c("by_id", "overall", "n_complete", "vars"))
  expect_equal(nrow(mp$by_id), 2)
  expect_equal(mp$overall$x$n_na, 1)
  expect_equal(mp$n_complete, 3)
})

test_that("ild_missing_pattern with no missing", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  mp <- ild_missing_pattern(x, vars = "x")
  expect_equal(mp$overall$x$n_na, 0)
  expect_equal(mp$n_complete, 4)
})
