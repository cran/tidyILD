msm_boot_data <- function(seed, n_id = 16L, n_obs = 8L) {
  set.seed(seed)
  d <- ild_simulate(n_id = n_id, n_obs_per = n_obs, seed = seed)
  ild_prepare(d, id = "id", time = "time")
}

msm_boot_ctl <- function() {
  lme4::lmerControl(
    optimizer = "bobyqa",
    check.conv.singular = "ignore",
    check.conv.hess = "ignore",
    check.conv.grad = "ignore"
  )
}

msm_boot_fit <- function(x, wcol = ".ipw") {
  x[[wcol]] <- runif(nrow(x), 0.92, 1.08)
  ctl <- msm_boot_ctl()
  f <- ild_lme(
    y ~ (1 | id),
    data = x,
    ar1 = FALSE,
    warn_no_ar1 = FALSE,
    warn_uncentered = FALSE,
    control = ctl
  )
  ild_ipw_refit(f, data = x, weights = wcol, control = ctl)
}

test_that("ild_msm_bootstrap fixed_weights returns expected structure", {
  x <- msm_boot_data(21L)
  fw <- msm_boot_fit(x)
  b <- ild_msm_bootstrap(
    fw,
    n_boot = 20L,
    weight_policy = "fixed_weights",
    seed = 71L,
    control = msm_boot_ctl()
  )
  expect_s3_class(b, "ild_msm_bootstrap")
  expect_equal(dim(b$replicates), c(20L, length(lme4::fixef(fw))))
  expect_equal(b$n_boot, 20L)
  expect_equal(b$n_success, 20L)
  expect_equal(names(b$estimate), b$term_names)
  prov <- attr(b, "ild_provenance", exact = TRUE)
  expect_true(is.list(prov))
  expect_equal(prov$object_type, "ild_msm_bootstrap")
  expect_true(any(vapply(prov$analysis_steps, function(s) identical(s$step, "ild_msm_bootstrap"), logical(1))))
})

test_that("tidy_ild_msm_bootstrap uses bootstrap_percentile interval_type", {
  x <- msm_boot_data(22L)
  fw <- msm_boot_fit(x)
  b <- ild_msm_bootstrap(
    fw,
    n_boot = 15L,
    seed = 32L,
    weight_policy = "fixed_weights",
    control = msm_boot_ctl()
  )
  expect_equal(b$n_success, 15L)
  td <- tidy_ild_msm_bootstrap(b, conf_level = 0.9)
  expect_s3_class(td, "tbl_df")
  expect_true(all(td$interval_type == "bootstrap_percentile"))
  expect_true(all(ild_tidy_schema()$required %in% names(td)))
})

test_that("ild_msm_bootstrap requires weights_fn for reestimate_weights", {
  x <- msm_boot_data(23L, n_id = 10L, n_obs = 6L)
  x$.ipw <- 1
  expect_error(
    ild_msm_bootstrap(
      NULL,
      formula = y ~ (1 | id),
      data = x,
      n_boot = 2L,
      weight_policy = "reestimate_weights",
      control = msm_boot_ctl()
    ),
    "weights_fn"
  )
})

test_that("ild_msm_bootstrap reestimate_weights runs with identity-like weights_fn", {
  x <- msm_boot_data(24L)
  fw <- msm_boot_fit(x)
  b <- ild_msm_bootstrap(
    fw,
    n_boot = 12L,
    seed = 51L,
    weight_policy = "reestimate_weights",
    weights_fn = function(z) z,
    control = msm_boot_ctl()
  )
  expect_equal(b$n_boot, 12L)
  expect_equal(b$n_success, 12L)
})

test_that("tidy_ild_model rejects ild_msm_bootstrap objects", {
  x <- msm_boot_data(25L, n_id = 12L, n_obs = 6L)
  fw <- msm_boot_fit(x)
  b <- ild_msm_bootstrap(
    fw,
    n_boot = 5L,
    seed = 11L,
    weight_policy = "fixed_weights",
    control = msm_boot_ctl()
  )
  expect_error(tidy_ild_model(b), "tidy_ild_msm_bootstrap")
})
