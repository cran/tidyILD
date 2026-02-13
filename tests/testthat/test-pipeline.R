test_that("Phase 1 pipeline: prepare -> summary -> center -> lag -> missing_pattern", {
  set.seed(42)
  d <- data.frame(
    id = rep(1:3, each = 5),
    time = rep(as.POSIXct(0:4 * 3600, origin = "1970-01-01"), 3),
    mood = rnorm(15),
    stress = rnorm(15)
  )
  # Introduce one missing
  d$mood[3] <- NA
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
  expect_true(is_ild(x))
  s <- ild_summary(x)
  expect_equal(s$n_units, 3)
  expect_equal(s$n_obs, 15)
  x <- ild_center(x, mood, stress)
  expect_true("mood_bp" %in% names(x))
  expect_true("mood_wp" %in% names(x))
  x <- ild_lag(x, mood, mode = "gap_aware", max_gap = 7200)
  expect_true("mood_lag1" %in% names(x))
  mp <- ild_missing_pattern(x, vars = "mood")
  expect_equal(mp$overall$mood$n_na, 1)
  expect_equal(ild_spacing_class(x), "regular-ish")
})

test_that("Phase 2 pipeline: prepare -> lme -> diagnostics -> plot", {
  d <- ild_simulate(n_id = 4, n_obs_per = 6, seed = 10)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  expect_true(inherits(fit, "lmerMod") && !is.null(attr(fit, "ild_data")))
  diag <- ild_diagnostics(fit, data = x)
  expect_s3_class(diag, "ild_diagnostics")
  p1 <- ild_plot(x, type = "trajectory", var = "y")
  p2 <- ild_plot(fit, type = "fitted")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})
