test_that("ild_acf on data returns acf tibble", {
  d <- ild_simulate(n_id = 5, n_obs_per = 10, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_acf(x, "y")
  expect_named(out, "acf")
  expect_s3_class(out$acf, "tbl_df")
  expect_true(all(c("lag", "acf") %in% names(out$acf)))
  expect_true(nrow(out$acf) >= 1)
})

test_that("ild_acf on fit returns residual ACF", {
  d <- ild_simulate(n_id = 4, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  out <- ild_acf(fit)
  expect_named(out, "acf")
  expect_true(all(c("lag", "acf") %in% names(out$acf)))
})

test_that("ild_acf with by_id adds acf_by_id", {
  d <- ild_simulate(n_id = 3, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_acf(x, "y", by_id = TRUE)
  expect_true("acf_by_id" %in% names(out))
  expect_true(is.list(out$acf_by_id))
  expect_equal(length(out$acf_by_id), 3)
})

test_that("ild_acf on fit without ild_data errors", {
  fit <- lme4::lmer(y ~ 1 + (1 | id), data = data.frame(id = rep(1:2, each = 5), y = rnorm(10)))
  expect_error(ild_acf(fit), "refit using ild_lme")
})
