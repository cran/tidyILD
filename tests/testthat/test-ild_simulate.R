test_that("ild_simulate returns data frame with id, time, y", {
  d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("id", "time", "y") %in% names(d)))
  expect_equal(nrow(d), 3 * 5)
})

test_that("ild_simulate n_time overrides n_obs_per", {
  d <- ild_simulate(n_id = 2, n_time = 7, seed = 1)
  expect_equal(nrow(d), 2 * 7)
})

test_that("ild_simulate with ar1 produces data", {
  d <- ild_simulate(n_id = 5, n_obs_per = 10, ar1 = 0.4, seed = 1)
  expect_equal(nrow(d), 50)
  expect_true(is.numeric(d$y))
})

test_that("ild_simulate wp_effect and bp_effect scale output", {
  d1 <- ild_simulate(n_id = 10, n_obs_per = 20, wp_effect = 0.1, bp_effect = 2, seed = 1)
  d2 <- ild_simulate(n_id = 10, n_obs_per = 20, wp_effect = 1, bp_effect = 0.1, seed = 1)
  expect_true(stats::var(d1$y) > stats::var(d2$y))
})
