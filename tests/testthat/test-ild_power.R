test_that("ild_power returns expected structure", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x + (1 | id),
    n_sim = 20L,
    n_id = 15L,
    n_obs_per = 10L,
    effect_size = 0.3,
    seed = 1L,
    verbose = FALSE
  )
  expect_named(res, c("power", "n_sim", "n_reject", "n_converged", "n_failed", "alpha", "test_term"))
  expect_equal(res$n_sim, 20L)
  expect_equal(res$test_term, "x")
  expect_true(is.numeric(res$power))
  expect_true(res$power >= 0 && res$power <= 1)
  expect_true(res$n_reject >= 0L && res$n_reject <= res$n_sim)
  expect_equal(res$n_converged + res$n_failed, res$n_sim)
  expect_equal(res$alpha, 0.05)
})

test_that("ild_power with effect_size 0 gives type I rate near alpha", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x + (1 | id),
    n_sim = 100L,
    n_id = 20L,
    n_obs_per = 12L,
    effect_size = 0,
    alpha = 0.05,
    seed = 42L,
    verbose = FALSE
  )
  # Type I rate should be roughly alpha; allow wide margin for simulation noise
  expect_true(res$power >= 0 && res$power <= 0.25)
})

test_that("ild_power with large effect_size gives high power", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x + (1 | id),
    n_sim = 50L,
    n_id = 40L,
    n_obs_per = 20L,
    effect_size = 1.5,
    seed = 123L,
    verbose = FALSE
  )
  expect_true(res$power >= 0.7)
})

test_that("ild_power with test_term explicitly set", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x + (1 | id),
    n_sim = 15L,
    n_id = 10L,
    n_obs_per = 8L,
    effect_size = 0.4,
    test_term = "x",
    seed = 2L,
    verbose = FALSE
  )
  expect_equal(res$test_term, "x")
})

test_that("ild_power errors on missing required args", {
  expect_error(
    ild_power(formula = y ~ x + (1 | id), n_sim = 10L, n_id = 5L, n_obs_per = 5L),
    "effect_size"
  )
  expect_error(
    ild_power(formula = y ~ x + (1 | id), n_sim = 10L, effect_size = 0.5, n_obs_per = 5L),
    "n_id"
  )
  expect_error(
    ild_power(formula = y ~ x + (1 | id), n_sim = 10L, n_id = 5L, effect_size = 0.5),
    "n_obs_per"
  )
})

test_that("ild_power errors on formula without predictor", {
  expect_error(
    ild_power(formula = y ~ 1 + (1 | id), n_sim = 5L, n_id = 5L, n_obs_per = 5L, effect_size = 0.5),
    "at least one predictor"
  )
})

test_that("ild_power with return_sims includes sim_results", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x + (1 | id),
    n_sim = 10L,
    n_id = 8L,
    n_obs_per = 6L,
    effect_size = 0.2,
    seed = 99L,
    return_sims = TRUE,
    verbose = FALSE
  )
  expect_true("sim_results" %in% names(res))
  expect_s3_class(res$sim_results, "tbl_df")
  expect_equal(nrow(res$sim_results), 10L)
  expect_true(all(c("sim", "estimate", "std_error", "p_value", "rejected", "converged") %in% names(res$sim_results)))
})

test_that("ild_power with ar1 = TRUE runs nlme path", {
  skip_on_cran()
  res <- ild_power(
    formula = y ~ x,
    n_sim = 8L,
    n_id = 10L,
    n_obs_per = 6L,
    effect_size = 0.5,
    ar1 = TRUE,
    seed = 77L,
    verbose = FALSE
  )
  expect_named(res, c("power", "n_sim", "n_reject", "n_converged", "n_failed", "alpha", "test_term"))
  expect_equal(res$test_term, "x")
  expect_true(is.numeric(res$power))
})
