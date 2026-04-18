test_that("ild_heterogeneity lmer random intercept", {
  d <- ild_simulate(n_id = 12, n_obs_per = 8, seed = 42)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_wp + y_bp + (1 | id), data = x)
  h <- ild_heterogeneity(fit)
  expect_s3_class(h, "ild_heterogeneity")
  expect_true(nrow(h$random_effects) >= 12L)
  expect_true(all(c("level_id", "term", "estimate_total") %in% names(h$random_effects)))
  tt <- ild_tidy(h)
  expect_equal(nrow(tt), nrow(h$random_effects))
  p <- ild_autoplot(h, type = "histogram", term = "(Intercept)")
  expect_s3_class(p, "ggplot")
})

test_that("ild_heterogeneity lmer random slope", {
  d <- ild_simulate(n_id = 15, n_obs_per = 10, seed = 43)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- suppressWarnings(
    ild_lme(y ~ y_wp + (y_wp | id), data = x,
            control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  )
  h <- ild_heterogeneity(fit, term = "y_wp")
  expect_true(any(h$summary$term == "y_wp"))
  expect_true(is.finite(h$summary$prop_gt_zero[h$summary$term == "y_wp"][1]))
  p <- ild_autoplot(h, type = "caterpillar", term = "y_wp")
  expect_s3_class(p, "ggplot")
})

test_that("ild_heterogeneity threshold scale raw", {
  d <- ild_simulate(n_id = 10, n_obs_per = 6, seed = 44)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_wp + (1 | id), data = x)
  h <- ild_heterogeneity(fit, threshold = -1e9, scale = "raw")
  expect_true(all(is.finite(h$summary$prop_gt_threshold) | is.na(h$summary$prop_gt_threshold)))
})

test_that("ild_diagnose attaches heterogeneity for lmer", {
  d <- ild_simulate(n_id = 8, n_obs_per = 5, seed = 45)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_wp + (1 | id), data = x)
  b <- ild_diagnose(fit)
  expect_true(!is.null(b$fit$heterogeneity))
  if (isTRUE(b$fit$heterogeneity$available)) {
    expect_s3_class(b$fit$heterogeneity$object, "ild_heterogeneity")
    p <- ild_autoplot(b, section = "fit", type = "heterogeneity", term = "(Intercept)")
    expect_s3_class(p, "ggplot")
  }
})

test_that("ild_heterogeneity_stratified returns rows or empty tibble", {
  d <- ild_simulate(n_id = 20, n_obs_per = 6, seed = 46)
  d$grp <- rep(c("A", "B"), length.out = nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  out <- ild_heterogeneity_stratified(
    y ~ y_wp + (1 | id),
    data = x,
    subgroup = "grp",
    min_n_id = 5L
  )
  expect_true("subgroup_level" %in% names(out) || nrow(out) == 0L)
})
