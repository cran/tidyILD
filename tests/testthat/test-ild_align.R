test_that("ild_align attaches aligned column with mean fun", {
  prim <- ild_prepare(
    data.frame(
      id = rep(1:2, each = 3),
      time = as.POSIXct(rep(c(0, 3600, 7200), 2), origin = "1970-01-01"),
      y = rnorm(6)
    ),
    id = "id", time = "time"
  )
  sec <- data.frame(
    id = rep(1:2, each = 4),
    time = as.POSIXct(rep(c(0, 1800, 3600, 5400), 2), origin = "1970-01-01"),
    hr = 70 + rnorm(8, 0, 2)
  )
  out <- ild_align(prim, sec, "hr", window = 3600, fun = "mean")
  expect_true("hr_aligned" %in% names(out))
  expect_equal(nrow(out), 6)
  expect_true(is.numeric(out$hr_aligned))
})

test_that("ild_align closest returns value from latest time in window", {
  prim <- ild_prepare(
    data.frame(id = 1, time = as.POSIXct(3600, origin = "1970-01-01"), y = 1),
    id = "id", time = "time"
  )
  sec <- data.frame(id = 1, time = as.POSIXct(c(0, 1800, 3600), origin = "1970-01-01"), v = c(10, 20, 30))
  out <- ild_align(prim, sec, "v", window = 3600, fun = "closest")
  expect_equal(out$v_aligned, 30)
})

test_that("ild_align errors when secondary missing required columns", {
  prim <- ild_prepare(
    data.frame(id = 1, time = as.POSIXct(0, origin = "1970-01-01"), y = 1),
    id = "id", time = "time"
  )
  sec <- data.frame(other_id = 1, time = as.POSIXct(0, origin = "1970-01-01"), hr = 60)
  expect_error(ild_align(prim, sec, "hr", window = 3600), "id column")
})
