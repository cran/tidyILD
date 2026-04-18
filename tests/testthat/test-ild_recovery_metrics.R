test_that("ild_recovery_metrics matches hand-computed bias rmse coverage", {
  sim <- tibble::tibble(
    estimate = c(1.0, 2.0, 10.0),
    std_error = c(1, 1, 1),
    converged = c(TRUE, TRUE, TRUE)
  )
  truth <- 2.0
  m <- ild_recovery_metrics(sim, truth = truth, level = 0.95)
  expect_equal(m$bias, mean(sim$estimate - truth))
  expect_equal(m$rmse, sqrt(mean((sim$estimate - truth)^2)))
  expect_equal(m$n, 3L)
  expect_equal(m$n_total, 3L)
  # truth=2 in Wald interval for est=1,2; not for est=10 (interval ~ [8.04, 11.96])
  expect_equal(m$coverage, 2 / 3)
})

test_that("ild_recovery_metrics filters converged_only", {
  sim <- tibble::tibble(
    estimate = c(1, 2),
    std_error = c(1, 1),
    converged = c(TRUE, FALSE)
  )
  m <- ild_recovery_metrics(sim, truth = 1, converged_only = TRUE)
  expect_equal(m$n, 1L)
  m2 <- ild_recovery_metrics(sim, truth = 1, converged_only = FALSE)
  expect_equal(m2$n, 2L)
})

test_that("ild_recovery_metrics errors on missing columns", {
  expect_error(ild_recovery_metrics(data.frame(a = 1), truth = 1), "sim_results must contain")
})

test_that("ild_recovery_metrics empty converged returns NA summaries", {
  sim <- tibble::tibble(
    estimate = 1,
    std_error = 1,
    converged = FALSE
  )
  m <- ild_recovery_metrics(sim, truth = 0, converged_only = TRUE)
  expect_equal(m$n, 0L)
  expect_true(is.na(m$bias))
})
