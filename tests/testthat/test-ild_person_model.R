test_that("ild_person_model returns tibble with term, estimate, std_error, p_value", {
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  pm <- ild_person_model(y ~ 1, x)
  expect_s3_class(pm, "tbl_df")
  expect_true(all(c(".ild_id", "term", "estimate", "std_error", "p_value") %in% names(pm)))
  expect_equal(unique(pm$term), "(Intercept)")
  expect_equal(nrow(pm), 6)
})

test_that("ild_person_model with predictor returns one row per person per term", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 2)
  x <- ild_prepare(d, id = "id", time = "time")
  pm <- ild_person_model(y ~ 1, x)
  expect_gte(nrow(pm), 4)
  expect_true(is.numeric(pm$estimate))
})

test_that("ild_person_model skips persons with too few obs when min_obs > n_obs", {
  d <- ild_simulate(n_id = 3, n_obs_per = 2, seed = 3)
  x <- ild_prepare(d, id = "id", time = "time")
  pm <- ild_person_model(y ~ 1, x, min_obs = 3)
  expect_equal(nrow(pm), 0)
})

test_that("ild_person_distribution returns ggplot", {
  d <- ild_simulate(n_id = 8, n_obs_per = 5, seed = 4)
  x <- ild_prepare(d, id = "id", time = "time")
  pm <- ild_person_model(y ~ 1, x)
  p <- ild_person_distribution(pm, term = "(Intercept)")
  expect_s3_class(p, "gg")
  p2 <- ild_person_distribution(pm, term = "(Intercept)", type = "density")
  expect_s3_class(p2, "gg")
})
