test_that("ild_fit backend brms matches ild_brms attributes", {
  skip_if_not_installed("brms")
  skip_on_cran()
  d <- ild_simulate(n_id = 5, n_obs_per = 4, seed = 201)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- suppressWarnings(ild_fit(
    y ~ y_bp + y_wp + (1 | id),
    data = x,
    backend = "brms",
    chains = 1,
    iter = 50,
    warmup = 25,
    refresh = 0,
    seed = 201,
    silent = 2,
    warn_uncentered = FALSE
  ))
  expect_s3_class(fit, "brmsfit")
  expect_true(!is.null(attr(fit, "ild_data", exact = TRUE)))
  expect_equal(attr(fit, "ild_posterior", exact = TRUE)$engine, "brms")
})

test_that("ild_fit errors when prior is set for lme4 backend", {
  d <- ild_simulate(n_id = 4, n_obs_per = 3, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_fit(y ~ (1 | id), data = x, backend = "lme4", prior = "not_null"),
    "prior is only used when backend"
  )
})

test_that("ild_fit errors when prior_template non-default for lme4", {
  d <- ild_simulate(n_id = 4, n_obs_per = 3, seed = 2)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_fit(y ~ (1 | id), data = x, backend = "lme4", prior_template = "weakly_informative"),
    "prior_template is only used when backend"
  )
})

test_that("ild_fit errors when correlation_class set for brms", {
  skip_if_not_installed("brms")
  d <- ild_simulate(n_id = 4, n_obs_per = 3, seed = 3)
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(
    ild_fit(y ~ (1 | id), data = x, backend = "brms", correlation_class = "CAR1"),
    "correlation_class is only used when backend"
  )
})
