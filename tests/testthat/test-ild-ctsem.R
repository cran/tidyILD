test_that("ild_ctsem fits and supports tidy/augment/diagnose/autoplot contracts", {
  skip_if_not_installed("ctsem")
  x <- ctsem_fixture_data(seed = 9401L, n_id = 1L, n_obs_per = 50L)
  fit <- suppressWarnings(
    ild_ctsem(
      data = x,
      outcome = "y",
      model_type = "stanct",
      iter = 200,
      chains = 1
    )
  )
  expect_s3_class(fit, "ild_fit_ctsem")
  expect_true(!is.null(attr(fit, "ild_data", exact = TRUE)))
  expect_true(!is.null(attr(fit, "ild_provenance", exact = TRUE)))

  td <- ild_tidy(fit)
  expect_true(all(ild_tidy_schema()$required %in% names(td)))
  expect_true(all(td$engine == "ctsem"))

  ag <- ild_augment(fit)
  expect_true(all(ild_augment_schema()$required %in% names(ag)))
  expect_equal(nrow(ag), nrow(attr(fit, "ild_data", exact = TRUE)))

  b <- ild_diagnose(fit)
  expect_s3_class(b, "ild_diagnostics_bundle")
  expect_no_error(validate_ild_diagnostics_bundle(b))
  expect_equal(b$meta$engine, "ctsem")

  expect_s3_class(ild_autoplot(fit, type = "fitted_vs_actual"), "ggplot")
  expect_s3_class(ild_autoplot(fit, type = "residual_time"), "ggplot")
  expect_s3_class(ild_autoplot(fit, type = "qq"), "ggplot")

  expect_s3_class(ild_autoplot(b, section = "residual", type = "acf"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "residual", type = "qq"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "data", type = "missingness"), "ggplot")
  expect_s3_class(ild_autoplot(b, section = "design", type = "coverage"), "ggplot")

  m <- ild_methods(fit)
  expect_true(grepl("ild_ctsem", m, fixed = TRUE) || grepl("continuous-time", m, fixed = TRUE))
})
