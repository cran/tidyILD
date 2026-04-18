test_that("ild_diagnostics_bundle has fixed slots and accepts NULL sections", {
  b <- ild_diagnostics_bundle(
    meta = list(engine = "test"),
    fit = list(ok = TRUE)
  )
  expect_s3_class(b, "ild_diagnostics_bundle")
  expect_named(b, names(ild_diagnostics_bundle()))
  expect_null(b$data)
  expect_equal(b$meta$engine, "test")
  expect_true(tibble::is_tibble(b$warnings))
  expect_true(tibble::is_tibble(b$guardrails))
  expect_equal(
    names(b$guardrails),
    c("rule_id", "section", "severity", "triggered", "message", "recommendation")
  )
})

test_that("guardrail_registry lists rules and matches bundle schema", {
  gr <- guardrail_registry()
  expect_s3_class(gr, "tbl_df")
  expect_true(all(c("rule_id", "section", "severity", "default_message", "default_recommendation") %in% names(gr)))
  expect_true(nrow(gr) >= 8L)
})

test_that("validate_ild_diagnostics_bundle rejects malformed list", {
  bad <- list(meta = NULL)
  expect_error(
    tidyILD:::validate_ild_diagnostics_bundle(bad),
    "exactly these names"
  )
  bad2 <- ild_diagnostics_bundle()
  bad2$guardrails <- tibble::tibble(rule = "x")
  expect_error(
    tidyILD:::validate_ild_diagnostics_bundle(bad2),
    "guardrails must have columns"
  )
})

test_that("ild_tidy_schema and ild_augment_schema return required and optional names", {
  ts <- ild_tidy_schema()
  expect_true(all(c("term", "conf_low", "conf_high", "engine") %in% ts$required))
  expect_true(all(c("rhat", "pd") %in% ts$optional))
  as <- ild_augment_schema()
  expect_true(all(c(".ild_id", ".outcome", ".resid_std", "engine") %in% as$required))
  expect_true(all(c(".fitted_lower", ".state") %in% as$optional))
})
