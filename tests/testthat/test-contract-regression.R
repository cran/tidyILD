# Contract regression: semantics (sections, guardrails, tidy/augment, autoplot, methods text)

test_that("contract: regular spacing + decomposed predictors — dense bundle, tidy/augment, plots, no guardrail methods", {
  fx <- fixture_contract_regular_decomposed()
  expect_equal(fx$spacing_class, "regular-ish")
  contract_assert_bundle_dense_lmer(fx$bundle)
  contract_assert_tidy_augment_lmer(fx$fit)
  contract_assert_autoplot_frequentist_core(fx$bundle)
  expect_equal(length(fx$expect_rule_ids), 0L)
  expect_false(fx$expect_methods_guardrail)
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_false(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
})

test_that("contract: irregular spacing + no AR1 — expected guardrails and methods text", {
  fx <- fixture_contract_irregular_no_ar1()
  expect_equal(fx$spacing_class, "irregular-ish")
  contract_assert_bundle_dense_lmer(fx$bundle)
  contract_assert_tidy_augment_lmer(fx$fit)
  contract_assert_autoplot_frequentist_core(fx$bundle)
  ids <- fx$bundle$guardrails$rule_id
  for (r in fx$expect_rule_ids) {
    expect_true(r %in% ids, info = r)
  }
  expect_true(fx$expect_methods_guardrail)
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_true(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
  gs <- tidyILD:::ild_guardrails_summary(fx$bundle)
  expect_true(gs$n >= 1L)
  expect_true(nzchar(gs$narrative))
})

test_that("contract: singular guardrail row merged — rule id and methods narrative", {
  fx <- fixture_contract_singular_guardrail()
  contract_assert_bundle_dense_lmer(fx$bundle)
  contract_assert_tidy_augment_lmer(fx$fit)
  expect_true("GR_SINGULAR_RANDOM_EFFECTS" %in% fx$bundle$guardrails$rule_id)
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_true(grepl("GR_SINGULAR_RANDOM_EFFECTS", m, fixed = TRUE) ||
    grepl("guardrail", m, ignore.case = TRUE))
})

test_that("contract: unstable IPW — causal weights plot + guardrail", {
  fx <- fixture_contract_ipw_unstable()
  contract_assert_bundle_dense_lmer(fx$bundle)
  contract_assert_tidy_augment_lmer(fx$fit)
  expect_true("GR_IPW_WEIGHTS_UNSTABLE" %in% fx$bundle$guardrails$rule_id)
  expect_s3_class(ild_autoplot(fx$bundle, section = "causal", type = "weights"), "ggplot")
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_true(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
})

test_that("contract: late dropout — merged guardrail + methods text", {
  fx <- fixture_contract_dropout_late()
  contract_assert_bundle_dense_lmer(fx$bundle)
  contract_assert_tidy_augment_lmer(fx$fit)
  expect_true("GR_DROPOUT_LATE_CONCENTRATION" %in% fx$bundle$guardrails$rule_id)
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_true(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
})

test_that("contract: brms bundle + merged poor mixing / low ESS guardrails", {
  skip_if_not_installed("brms")
  skip_on_cran()
  fx <- fixture_contract_brms_poor_mixing_merged()
  expect_true(!is.null(fx$bundle$meta))
  expect_true(!is.null(fx$bundle$fit))
  expect_true(!is.null(attr(fx$bundle, "ild_fit", exact = TRUE)))
  td <- ild_tidy(fx$fit)
  expect_true(all(contract_tidy_required() %in% names(td)))
  for (r in fx$expect_rule_ids) {
    expect_true(r %in% fx$bundle$guardrails$rule_id, info = r)
  }
  expect_s3_class(ild_autoplot(fx$bundle, section = "fit", type = "convergence"), "ggplot")
  p <- ild_autoplot(fx$bundle, section = "predictive", type = "ppc", ndraws = 10)
  expect_s3_class(p, "ggplot")
  m <- ild_methods(fx$fit, bundle = fx$bundle)
  expect_true(grepl("Methodological cautions (tidyILD guardrails)", m, fixed = TRUE))
})

test_that("contract: ctsem backend satisfies tidy/augment/bundle contracts", {
  skip_if_not_installed("ctsem")
  x <- ctsem_fixture_data(seed = 9410L, n_id = 1L, n_obs_per = 45L)
  fit <- suppressWarnings(
    ild_ctsem(
      data = x,
      outcome = "y",
      model_type = "stanct",
      iter = 200,
      chains = 1
    )
  )
  td <- ild_tidy(fit)
  expect_true(all(contract_tidy_required() %in% names(td)))
  ag <- ild_augment(fit)
  expect_true(all(contract_augment_required() %in% names(ag)))
  b <- ild_diagnose(fit)
  expect_s3_class(b, "ild_diagnostics_bundle")
  expect_no_error(validate_ild_diagnostics_bundle(b))
  expect_s3_class(ild_autoplot(b, section = "fit", type = "convergence"), "ggplot")
  m <- ild_methods(fit, bundle = b)
  expect_true(grepl("ild_ctsem", m, fixed = TRUE) || grepl("continuous-time", m, fixed = TRUE))
})
