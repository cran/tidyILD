test_that("ild_tvem fits on simulated ILD and has expected class and meta", {
  set.seed(1)
  d <- ild_simulate(n_id = 8, n_obs_per = 12, seed = 1)
  d$x <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  tv <- ild_tvem(x, "y", "x", k = 5L, re_id = TRUE)
  expect_true(inherits(tv, "tidyild_tvem"))
  expect_true(inherits(tv, "gam"))
  meta <- attr(tv, "ild_tvem_meta")
  expect_named(meta, c("outcome", "predictor", "time_var", "k", "re_id"))
  expect_equal(meta$outcome, "y")
  expect_equal(meta$predictor, "x")
  expect_equal(meta$time_var, ".ild_time_num")
  expect_equal(meta$k, 5L)
})

test_that("ild_tvem_plot returns a ggplot", {
  set.seed(1)
  d <- ild_simulate(n_id = 6, n_obs_per = 10, seed = 1)
  d$x <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  tv <- ild_tvem(x, "y", "x", k = 5L, re_id = TRUE)
  p <- ild_tvem_plot(tv, n_grid = 50L)
  expect_s3_class(p, "ggplot")
})

test_that("ild_tvem runs in under 2 seconds on small data", {
  set.seed(1)
  d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 1)
  d$x <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  elapsed <- system.time(ild_tvem(x, "y", "x", k = 5L))["elapsed"]
  expect_lt(elapsed, 2)
})
