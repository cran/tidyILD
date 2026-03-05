test_that("ild_circadian returns ggplot when .ild_time is POSIXct", {
  d <- ild_simulate(n_id = 5, n_obs_per = 12, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_circadian(x, y)
  expect_s3_class(p, "gg")
  p2 <- ild_circadian(x, y, type = "line")
  expect_s3_class(p2, "gg")
})

test_that("ild_circadian errors when .ild_time is not POSIXct", {
  d <- data.frame(id = rep(1:2, each = 5), time = 1:5, y = rnorm(10))
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(ild_circadian(x, y), "POSIXct")
})

test_that("ild_circadian errors on missing variable", {
  x <- ild_prepare(ild_simulate(n_id = 2, n_obs_per = 3, seed = 1), id = "id", time = "time")
  expect_error(ild_circadian(x, nonexistent), "not found")
})
