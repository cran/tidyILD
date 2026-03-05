test_that("ild_missing_bias returns list with estimate, p_value, message", {
  set.seed(1)
  d <- ild_simulate(n_id = 15, n_obs_per = 8, seed = 1)
  d$stress <- rnorm(nrow(d))
  d$mood <- d$y
  d$mood[sample(nrow(d), 20)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_missing_bias(x, "mood", "stress")
  expect_named(out, c("predictor", "estimate", "std_error", "p_value", "message"))
  expect_equal(out$predictor, "stress")
  expect_true(is.numeric(out$estimate))
  expect_true(is.numeric(out$p_value))
  expect_true(is.character(out$message))
})

test_that("ild_missing_bias with no missingness returns NA and message", {
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 1)
  d$stress <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_missing_bias(x, "y", "stress")
  expect_true(is.na(out$estimate))
  expect_true(grepl("No missingness", out$message))
})

test_that("ild_missing_bias errors on missing variable names", {
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(ild_missing_bias(x, "nonexistent", "y"), "not found")
  expect_error(ild_missing_bias(x, "y", "nonexistent"), "not found")
})
