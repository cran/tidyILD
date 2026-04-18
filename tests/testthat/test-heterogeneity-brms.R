test_that("ild_heterogeneity brmsfit matches coef summary structure", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 201)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_brms(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    chains = 1, iter = 80, warmup = 40,
    refresh = 0, seed = 201, silent = 2,
    backend = "rstan",
    warn_uncentered = FALSE
  )
  h <- ild_heterogeneity(fit)
  expect_s3_class(h, "ild_heterogeneity")
  expect_identical(h$meta$engine, "brms")
  expect_true(nrow(h$random_effects) >= 6L)
  expect_true(all(is.finite(h$random_effects$estimate_total)))
  p <- ild_autoplot(h, type = "caterpillar", term = "Intercept")
  expect_s3_class(p, "ggplot")
})
