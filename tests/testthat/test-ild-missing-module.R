test_that("ild_missing_compliance monotone dropout and expected occasions", {
  d <- ild_simulate(n_id = 2, n_obs_per = 5, seed = 1201L)
  x <- ild_prepare(d, id = "id", time = "time")
  w1 <- x$.ild_id == 1L
  x$y[w1] <- NA
  x$y[x$.ild_id == 1L & x$.ild_seq <= 2L] <- 1
  cm <- ild_missing_compliance(x, outcome = "y", expected_occasions = 5L)
  expect_true(all(c("n_rows", "monotone_missing", "pct_of_expected") %in% names(cm)))
  m1 <- cm$monotone_missing[cm$.ild_id == 1L][1L]
  expect_true(isTRUE(m1))
  m2 <- cm$monotone_missing[cm$.ild_id == 2L][1L]
  expect_true(is.na(m2))
})

test_that("ild_missing_hazard_first sums to sensible risk set", {
  d <- ild_simulate(n_id = 20, n_obs_per = 8, seed = 1202L)
  x <- ild_prepare(d, id = "id", time = "time")
  hz <- ild_missing_hazard_first(x, outcome = "y")
  expect_true(all(hz$n_at_risk >= hz$n_missing, na.rm = TRUE))
  expect_true(all(hz$hazard <= 1, na.rm = TRUE))
})

test_that("ild_missing_pattern joins compliance when outcome set", {
  d <- ild_simulate(n_id = 8, n_obs_per = 10, seed = 1203L)
  d$y[sample(nrow(d), 12)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  mp <- ild_missing_pattern(x, vars = "y", outcome = "y", expected_occasions = 10L)
  expect_true("pct_nonmissing_outcome" %in% names(mp$by_id))
  expect_true("longest_run_observed" %in% names(mp$by_id))
})

test_that("ild_missingness_report returns expected components", {
  d <- ild_simulate(n_id = 12, n_obs_per = 9, seed = 1204L)
  d$x <- rnorm(nrow(d))
  d$y[sample(nrow(d), 15)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  r <- ild_missingness_report(
    x,
    outcome = "y",
    predictors = "x",
    fit_missing_model = TRUE,
    cohort_plot = FALSE
  )
  expect_s3_class(r, "ild_missingness_report")
  expect_true(all(c("compliance", "pattern", "cohort", "hazard", "flags", "snippets") %in% names(r)))
  expect_true(is.logical(r$flags$dropout_late_pooled))
  expect_true(length(r$snippets) >= 1L)
})
