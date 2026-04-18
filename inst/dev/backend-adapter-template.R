# =============================================================================
# tidyILD — backend adapter template (NOT part of the package build)
# =============================================================================
#
# Copy sections into new R files under R/ when adding an estimation engine.
# Normative checklist: inst/dev/DEVELOPER_CONTRACTS.md §4 ("Backend adapter
# checklist"). This file is a **skeleton** only; replace MYENGINE, MYCLASS,
# ild_myfit(), etc. with real names.
#
# Obligations (avoid fragmentation across backends):
#   1. Tidier       — ild_tidy() method, ild_tidy_schema() columns
#   2. Augmenter    — ild_augment() method, ild_augment_schema() columns
#   3. Diagnose     — ild_diagnose() method → ild_diagnostics_bundle
#   4. Bundle shape — validate_ild_diagnostics_bundle; semantic fit/predictive
#   5. Autoplot     — bundle must carry attr(ild_fit), attr(ild_data)
#   6. Provenance   — attr(fit, "ild_provenance") for ild_methods()
#   7. Guardrails   — evaluate_guardrails_fit(fit_diag, engine = "...")
#   8. Tests        — schema + bundle + optional rule_id regression
#   9. Docs         — roxygen, DEVELOPER_CONTRACTS if slots change, NEWS
#
# Reference implementations in this package:
#   - Frequentist: R/ild_model_tidiers.R (tidy/augment), R/ild_diagnose_methods.R
#   - Bayesian:    R/ild_brms_s3.R, R/ild_brms.R (provenance), R/ild_diagnose_fillers.R
#
# =============================================================================

## ---------------------------------------------------------------------------
## 0. File layout (typical new engine)
## ---------------------------------------------------------------------------
#
# R/ild_myengine.R          — public fitter ild_myfit(); ild_data + ild_provenance
# R/ild_myengine_s3.R       — ild_tidy.MYCLASS, ild_augment.MYCLASS, ild_autoplot.brmsfit-style if needed
# R/ild_diagnose_methods.R  — add ild_diagnose.MYCLASS (or separate file + Collate)
# R/ild_diagnose_fillers.R  — add fill_diagnostics_fit_MYENGINE(), predictive, or source myengine_fillers.R
#
# tests/testthat/test-myengine.R — schema + bundle + smoke
# inst/dev/DEVELOPER_CONTRACTS.md — update §1 if fit/predictive semantics change
#

## ---------------------------------------------------------------------------
## 1. Tidier — conform to ild_tidy_schema()
## ---------------------------------------------------------------------------
#
# Required columns (see ?ild_tidy_schema): term, component, effect_level,
# estimate, std_error, conf_low, conf_high, statistic, p_value, interval_type,
# engine, model_class. Optional: rhat, ess_bulk, ess_tail, pd, rope_low, rope_high.
#
# Register in R/ild_generics.R:
#   @export ild_tidy.MYCLASS
#   ild_tidy.MYCLASS <- function(x, ...) { ... }
#
# Validate in tests:
#   expect_true(all(ild_tidy_schema()$required %in% names(out)))
#

if (FALSE) {
  #' @rdname ild_tidy
  #' @method ild_tidy MYCLASS
  #' @export
  ild_tidy.MYCLASS <- function(x, ...) {
    # Assemble a tibble; set engine = "myengine", model_class = class(x)[1]
    # Map terms to component / effect_level per DEVELOPER_CONTRACTS.md §2.3–2.4
    out <- tibble::tibble(
      term = character(),
      component = character(),
      effect_level = character(),
      estimate = double(),
      std_error = double(),
      conf_low = double(),
      conf_high = double(),
      statistic = double(),
      p_value = double(),
      interval_type = character(),
      engine = "myengine",
      model_class = class(x)[1]
    )
    out
  }
}

## ---------------------------------------------------------------------------
## 2. Augmenter — conform to ild_augment_schema()
## ---------------------------------------------------------------------------
#
# Required: .ild_id, .ild_time, .outcome, .fitted, .resid, .resid_std,
#           engine, model_class
# The fitted object must have attr(x, "ild_data") from your fitter (validate_ild).
#
# .resid_std: use residuals(x, type = "pearson") only when length-matched; else NA.
#

if (FALSE) {
  #' @rdname ild_augment
  #' @method ild_augment MYCLASS
  #' @export
  ild_augment.MYCLASS <- function(x, ...) {
    dat <- attr(x, "ild_data", exact = TRUE)
    if (is.null(dat)) {
      stop("MYCLASS fits require attr(fit, \"ild_data\") from ild_myfit().", call. = FALSE)
    }
    # Build tibble aligned to rows of dat; set engine / model_class columns
    out <- tibble::tibble(
      .ild_id = dat$.ild_id,
      .ild_time = dat$.ild_time,
      .outcome = NA_real_,
      .fitted = NA_real_,
      .resid = NA_real_,
      .resid_std = NA_real_,
      engine = "myengine",
      model_class = class(x)[1]
    )
    out
  }
}

## ---------------------------------------------------------------------------
## 3. ild_diagnose — ild_diagnostics_bundle only
## ---------------------------------------------------------------------------
#
# Pattern: mirror R/ild_diagnose_methods.R (lmerMod / lme / brmsfit).
# Use ild_diagnostics_bundle() constructor; slots in ILD_DIAGNOSTICS_BUNDLE_SLOTS.
# Reuse fill_diagnostics_data(), fill_diagnostics_design(), etc. where possible.
# Add fill_diagnostics_fit_myengine() in ild_diagnose_fillers.R for nested
# fit semantics (convergence, rank, optimizer, …) parallel to other engines.
# End with:
#   bundle$summary_text <- c(build_diagnostics_bundle_summary(bundle), ...)
#   bundle <- enrich_bundle_semantic_sections(bundle)
#   attr(bundle, "ild_fit") <- object
#   attr(bundle, "ild_data") <- data
#
# Guardrails:
#   bundle$guardrails <- guardrails_bind(
#     evaluate_guardrails_fit(object, fit_diag, engine = "myengine"),
#     evaluate_guardrails_contextual(object, data, bundle, engine = "myengine")
#   )
# Extend match.arg(engine) in evaluate_guardrails_fit() and
# evaluate_guardrails_contextual() when adding a new engine id (see
# R/ild_guardrail_registry.R). Until then, use the closest existing engine
# only for prototyping.
#
# fit_diag for brms-like rules: fit_bundle_flat_for_guardrails(bundle$fit, engine = "...")
# reads nested or flat fit slices (see R/ild_diagnose_fillers.R).
#

if (FALSE) {
  #' @rdname ild_diagnose
  #' @method ild_diagnose MYCLASS
  #' @export
  ild_diagnose.MYCLASS <- function(object, data = NULL, ...) {
    data <- if (is.null(data)) attr(object, "ild_data", exact = TRUE) else data
    validate_ild(data)
    preds <- NULL # e.g. all.vars(stats::terms(stats::formula(object)))
    outcome_vars <- ild_diagnose_outcome_vars(object, data, preds)
    fit_diag <- fill_diagnostics_fit_myengine(object)
    b <- ild_diagnostics_bundle(
      meta = list(engine = "myengine", n_obs = nrow(data), n_id = length(unique(data[[".ild_id"]]))),
      data = fill_diagnostics_data(data, outcome_vars = outcome_vars),
      design = fill_diagnostics_design(data, vars = if (length(preds)) preds else NULL),
      fit = fit_diag,
      residual = fill_diagnostics_residual_myengine(object, data),
      predictive = fill_diagnostics_predictive_myengine(object, data),
      missingness = fill_diagnostics_missingness_section(data, object, preds),
      causal = fill_diagnostics_causal(data, causal_detail = FALSE),
      warnings = tibble::tibble(),
      guardrails = guardrails_empty_tibble()
    )
    b$guardrails <- guardrails_bind(
      evaluate_guardrails_fit(object, fit_diag, engine = "myengine"),
      evaluate_guardrails_contextual(object, data, b, engine = "myengine")
    )
    b$summary_text <- build_diagnostics_bundle_summary(b)
    b <- enrich_bundle_semantic_sections(b)
    attr(b, "ild_fit") <- object
    attr(b, "ild_data") <- data
    b
  }
}

## ---------------------------------------------------------------------------
## 4. Autoplot — compatibility through the bundle
## ---------------------------------------------------------------------------
#
# Users call ild_autoplot(bundle, section = ..., type = ...).
# Implementations live in R/ild_autoplot_bundle.R: add plotters that read
# bundle$fit$convergence, bundle$predictive$simulation_checks$ppc, etc.
# Do not require users to pass the fit again if attributes are set (see
# ?ild_diagnostics_bundle).
#
# For a new section-specific plot, add a branch in ild_autoplot.ild_diagnostics_bundle
# and a plot_bundle_*() helper.
#

## ---------------------------------------------------------------------------
## 5. Provenance — ild_methods() and reporting
## ---------------------------------------------------------------------------
#
# After fitting:
#   attr(fit, "ild_data") <- data
#   attr(fit, "ild_provenance") <- ild_new_analysis_provenance(
#     step = "ild_myfit",
#     args = list(formula = formula, ...),
#     outputs = list(engine = "myengine", ...)
#   )
#
# See R/ild_provenance.R and R/ild_methods.R.
#

## ---------------------------------------------------------------------------
## 6. Tests (minimum)
## ---------------------------------------------------------------------------
#
# - ild_tidy: all ild_tidy_schema()$required columns present (or documented waiver).
# - ild_augment: all ild_augment_schema()$required columns present.
# - ild_diagnose: expect_s3_class(b, "ild_diagnostics_bundle");
#                 expect_no_error(validate_ild_diagnostics_bundle(b))
# - Guardrails: if engine participates in fit-level rules, add a row to
#   tests/testthat/test-guardrails-triggers.R (deterministic fit_diag fixture)
#   or extend test-contract-regression.R patterns.
# - Optional: tests/testthat/helper-contract-fixtures.R for merged contract tests.
#

## ---------------------------------------------------------------------------
## 7. Documentation updates
## ---------------------------------------------------------------------------
#
# - @rdname shared with sibling methods where possible.
# - Describe engine-specific bundle slots in ild_diagnostics_bundle Details or
#   a dedicated topic page.
# - NEWS.md: mention new engine and any new guardrail rule_ids.
# - If normative bundle/schema text changes, edit inst/dev/DEVELOPER_CONTRACTS.md
#   (vignette developer-contracts embeds it).
#

## ---------------------------------------------------------------------------
## 8. NAMESPACE / Collate
## ---------------------------------------------------------------------------
#
# - Export new S3 methods via roxygen2.
# - Add new source files to Collate in DESCRIPTION **before** files that call them.
#

NULL
