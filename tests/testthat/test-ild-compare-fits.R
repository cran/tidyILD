test_that("ild_compare_fits returns AIC/BIC for two lmer models", {
  d <- ild_simulate(n_id = 8, n_obs_per = 10, seed = 904L)
  d$x <- rnorm(nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  f1 <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  f2 <- ild_lme(y ~ x + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  cmp <- ild_compare_fits(list(simple = f1, with_x = f2))
  expect_true(all(c("model", "aic", "bic", "n_obs", "converged") %in% names(cmp)))
  expect_equal(nrow(cmp), 2L)
  expect_true(is.finite(cmp$aic[1]) && is.finite(cmp$aic[2]))
})

test_that("ild_compare_fits passes guardrail_bundles through", {
  d <- ild_simulate(n_id = 5, n_obs_per = 8, seed = 905L)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  b <- ild_diagnose(fit, data = x, type = "qq")
  cmp <- ild_compare_fits(list(a = fit), guardrail_bundles = list(b))
  expect_true("n_guardrails" %in% names(cmp))
  expect_equal(cmp$n_guardrails[1], nrow(b$guardrails))
})

test_that("ild_brms_dynamics_formula returns formula with lag and notes", {
  tpl <- ild_brms_dynamics_formula("y", "x_lag1", id_var = "id")
  expect_s3_class(tpl$formula, "formula")
  expect_true(nzchar(tpl$notes))
})
