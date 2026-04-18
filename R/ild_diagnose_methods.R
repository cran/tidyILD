# ild_diagnose() S3 methods — all return ild_diagnostics_bundle

#' @keywords internal
#' @noRd
ild_model_predictor_names <- function(object, data) {
  f <- tryCatch(stats::formula(object), error = function(e) NULL)
  if (is.null(f)) return(character())
  intersect(all.vars(f), names(data))
}

#' @keywords internal
#' @noRd
ild_diagnose_outcome_vars <- function(object, data, preds) {
  rn <- tryCatch(ild_response_name(object), error = function(e) NULL)
  ov <- unique(c(stats::na.omit(c(rn, preds))))
  ov[ov %in% names(data)]
}

#' @rdname ild_diagnose
#' @method ild_diagnose lmerMod
#' @export
ild_diagnose.lmerMod <- function(object,
                                 data = NULL,
                                 type = c("residual_acf", "residual_time", "qq"),
                                 by_id = TRUE,
                                 missing_model = FALSE,
                                 missing_model_predictors = NULL,
                                 causal_detail = FALSE,
                                 balance = FALSE,
                                 balance_treatment = NULL,
                                 balance_covariates = NULL,
                                 balance_weights_col = ".ipw_treat",
                                 balance_by_occasion = FALSE,
                                 ...) {
  if (is.null(data)) data <- attr(object, "ild_data", exact = TRUE)
  if (is.null(data)) stop("Provide ILD data or refit with ild_lme() so the model has ild_data.", call. = FALSE)
  validate_ild(data)
  type <- match.arg(type, choices = c("residual_acf", "residual_time", "qq"), several.ok = TRUE)
  preds <- ild_model_predictor_names(object, data)
  outcome_vars <- ild_diagnose_outcome_vars(object, data, preds)
  fd <- fill_diagnostics_fit_lmerMod(object)
  wg <- collect_freq_warnings_guardrails(object, fd)
  miss <- fill_diagnostics_missingness_section(data, vars = preds)
  if (isTRUE(missing_model)) {
    mpreds <- if (is.null(missing_model_predictors)) preds else missing_model_predictors
    rn <- tryCatch(ild_response_name(object), error = function(e) NULL)
    if (length(mpreds) > 0L && !is.null(rn) && rn %in% names(data)) {
      mm <- tryCatch(ild_missing_model(data, outcome = rn, predictors = mpreds), error = function(e) NULL)
      if (!is.null(mm)) {
        miss$missing_model_diagnostic <- list(
          outcome = mm$outcome,
          predictors = mm$predictors,
          tidy = mm$tidy,
          message = mm$message
        )
      }
    } else {
      miss$missing_model_diagnostic <- list(
        message = "missing_model requires a response column in data and at least one predictor (set missing_model_predictors if formula has none)."
      )
    }
  }
  bundle <- ild_diagnostics_bundle(
    meta = list(
      engine = "lmer",
      n_obs = nrow(data),
      n_id = length(unique(data[[".ild_id"]])),
      ar1 = isTRUE(attr(object, "ild_ar1", exact = TRUE))
    ),
    data = fill_diagnostics_data(data, outcome_vars = outcome_vars),
    design = fill_diagnostics_design(data, vars = if (length(preds)) preds else NULL),
    fit = fd,
    residual = fill_diagnostics_residual_frequentist(object, data, type, by_id),
    predictive = fill_diagnostics_predictive_frequentist(object, data),
    missingness = miss,
    causal = fill_diagnostics_causal(
      data,
      causal_detail = causal_detail,
      balance = balance,
      balance_treatment = balance_treatment,
      balance_covariates = balance_covariates,
      balance_weights_col = balance_weights_col,
      balance_by_occasion = balance_by_occasion
    ),
    warnings = wg$warnings,
    guardrails = guardrails_empty_tibble(),
    summary_text = character()
  )
  bundle$design$occasion_imbalance <- bundle$data$obs_per_id
  bundle$guardrails <- guardrails_bind(
    evaluate_guardrails_fit(object, fd, engine = "lmer"),
    evaluate_guardrails_contextual(object, data, bundle, engine = "lmer")
  )
  bundle$summary_text <- build_diagnostics_bundle_summary(bundle)
  bundle <- enrich_bundle_semantic_sections(bundle)
  attr(bundle, "ild_fit") <- object
  attr(bundle, "ild_data") <- data
  bundle
}

#' @rdname ild_diagnose
#' @method ild_diagnose lme
#' @export
ild_diagnose.lme <- function(object,
                             data = NULL,
                             type = c("residual_acf", "residual_time", "qq"),
                             by_id = TRUE,
                             missing_model = FALSE,
                             missing_model_predictors = NULL,
                             causal_detail = FALSE,
                             balance = FALSE,
                             balance_treatment = NULL,
                             balance_covariates = NULL,
                             balance_weights_col = ".ipw_treat",
                             balance_by_occasion = FALSE,
                             ...) {
  if (is.null(data)) data <- attr(object, "ild_data", exact = TRUE)
  if (is.null(data)) stop("Provide ILD data or refit with ild_lme() so the model has ild_data.", call. = FALSE)
  validate_ild(data)
  type <- match.arg(type, choices = c("residual_acf", "residual_time", "qq"), several.ok = TRUE)
  preds <- ild_model_predictor_names(object, data)
  outcome_vars <- ild_diagnose_outcome_vars(object, data, preds)
  fd <- fill_diagnostics_fit_lme(object)
  wg <- collect_freq_warnings_guardrails(object, fd)
  miss <- fill_diagnostics_missingness_section(data, vars = preds)
  if (isTRUE(missing_model)) {
    mpreds <- if (is.null(missing_model_predictors)) preds else missing_model_predictors
    rn <- tryCatch(ild_response_name(object), error = function(e) NULL)
    if (length(mpreds) > 0L && !is.null(rn) && rn %in% names(data)) {
      mm <- tryCatch(ild_missing_model(data, outcome = rn, predictors = mpreds), error = function(e) NULL)
      if (!is.null(mm)) {
        miss$missing_model_diagnostic <- list(
          outcome = mm$outcome,
          predictors = mm$predictors,
          tidy = mm$tidy,
          message = mm$message
        )
      }
    } else {
      miss$missing_model_diagnostic <- list(
        message = "missing_model requires a response column in data and at least one predictor (set missing_model_predictors if formula has none)."
      )
    }
  }
  bundle <- ild_diagnostics_bundle(
    meta = list(
      engine = "lme",
      n_obs = nrow(data),
      n_id = length(unique(data[[".ild_id"]])),
      ar1 = isTRUE(attr(object, "ild_ar1", exact = TRUE))
    ),
    data = fill_diagnostics_data(data, outcome_vars = outcome_vars),
    design = fill_diagnostics_design(data, vars = if (length(preds)) preds else NULL),
    fit = fd,
    residual = fill_diagnostics_residual_frequentist(object, data, type, by_id),
    predictive = fill_diagnostics_predictive_frequentist(object, data),
    missingness = miss,
    causal = fill_diagnostics_causal(
      data,
      causal_detail = causal_detail,
      balance = balance,
      balance_treatment = balance_treatment,
      balance_covariates = balance_covariates,
      balance_weights_col = balance_weights_col,
      balance_by_occasion = balance_by_occasion
    ),
    warnings = wg$warnings,
    guardrails = guardrails_empty_tibble(),
    summary_text = character()
  )
  bundle$design$occasion_imbalance <- bundle$data$obs_per_id
  bundle$guardrails <- guardrails_bind(
    evaluate_guardrails_fit(object, fd, engine = "lme"),
    evaluate_guardrails_contextual(object, data, bundle, engine = "lme")
  )
  bundle$summary_text <- build_diagnostics_bundle_summary(bundle)
  bundle <- enrich_bundle_semantic_sections(bundle)
  attr(bundle, "ild_fit") <- object
  attr(bundle, "ild_data") <- data
  bundle
}

#' @rdname ild_diagnose
#' @method ild_diagnose brmsfit
#' @export
ild_diagnose.brmsfit <- function(object,
                                 data = NULL,
                                 type = c("all", "convergence", "sampler", "ppc"),
                                 by_id = TRUE,
                                 ppc_ndraws = 500L,
                                 missing_model = FALSE,
                                 missing_model_predictors = NULL,
                                 causal_detail = FALSE,
                                 balance = FALSE,
                                 balance_treatment = NULL,
                                 balance_covariates = NULL,
                                 balance_weights_col = ".ipw_treat",
                                 balance_by_occasion = FALSE,
                                 ...) {
  if (is.null(attr(object, "ild_data", exact = TRUE))) {
    stop("ild_diagnose() for brmsfit requires ild_brms() (ild_data attribute missing).", call. = FALSE)
  }
  rlang::check_installed("brms")
  data <- attr(object, "ild_data", exact = TRUE)
  validate_ild(data)
  if (length(type) == 1L) {
    type <- match.arg(type, choices = c("all", "convergence", "sampler", "ppc"))
  }
  types <- if (identical(type, "all")) {
    c("convergence", "sampler", "ppc")
  } else {
    match.arg(type, c("convergence", "sampler", "ppc"), several.ok = TRUE)
  }
  preds <- ild_model_predictor_names(object, data)
  outcome_vars <- ild_diagnose_outcome_vars(object, data, preds)
  ffit <- fill_diagnostics_fit_brms(object)
  pred_sec <- if ("ppc" %in% types) {
    fill_diagnostics_predictive_brms(object, ppc_ndraws = ppc_ndraws)
  } else {
    fill_diagnostics_predictive_brms_augment_only(object)
  }
  wg <- collect_brms_warnings_guardrails(object, ffit)
  miss <- fill_diagnostics_missingness_section(data, vars = preds)
  if (isTRUE(missing_model)) {
    mpreds <- if (is.null(missing_model_predictors)) preds else missing_model_predictors
    rn <- tryCatch(ild_response_name(object), error = function(e) NULL)
    if (length(mpreds) > 0L && !is.null(rn) && rn %in% names(data)) {
      mm <- tryCatch(ild_missing_model(data, outcome = rn, predictors = mpreds), error = function(e) NULL)
      if (!is.null(mm)) {
        miss$missing_model_diagnostic <- list(
          outcome = mm$outcome,
          predictors = mm$predictors,
          tidy = mm$tidy,
          message = mm$message
        )
      }
    } else {
      miss$missing_model_diagnostic <- list(
        message = "missing_model requires a response column in data and at least one predictor (set missing_model_predictors if formula has none)."
      )
    }
  }
  bundle <- ild_diagnostics_bundle(
    meta = list(engine = "brms", brms_types = types, n_obs = nrow(data), n_id = length(unique(data[[".ild_id"]]))),
    data = fill_diagnostics_data(data, outcome_vars = outcome_vars),
    design = fill_diagnostics_design(data, vars = if (length(preds)) preds else NULL),
    fit = ffit,
    residual = fill_diagnostics_residual_brms(object),
    predictive = pred_sec,
    missingness = miss,
    causal = fill_diagnostics_causal(
      data,
      causal_detail = causal_detail,
      balance = balance,
      balance_treatment = balance_treatment,
      balance_covariates = balance_covariates,
      balance_weights_col = balance_weights_col,
      balance_by_occasion = balance_by_occasion
    ),
    warnings = wg$warnings,
    guardrails = guardrails_empty_tibble(),
    summary_text = character()
  )
  bundle$design$occasion_imbalance <- bundle$data$obs_per_id
  bundle$guardrails <- guardrails_bind(
    evaluate_guardrails_fit(object, ffit, engine = "brms"),
    evaluate_guardrails_contextual(object, data, bundle, engine = "brms")
  )
  bundle$summary_text <- c(
    build_diagnostics_bundle_summary(bundle),
    build_diagnostics_bundle_summary_brms(bundle, types)
  )
  bundle <- enrich_bundle_semantic_sections(bundle)
  attr(bundle, "ild_fit") <- object
  attr(bundle, "ild_data") <- data
  bundle
}

#' @keywords internal
#' @noRd
build_diagnostics_bundle_summary_brms <- function(bundle, types) {
  parts <- character()
  ps <- bundle$fit$posterior_summary$ild_posterior %||% bundle$fit$ild_posterior
  if ("sampler" %in% types && !is.null(ps)) {
    parts <- c(parts, sprintf(
      "Sampler: %s chains, iter = %s, warmup = %s, divergent = %s.",
      ps$chains, ps$iter, ps$warmup,
      if (is.na(ps$n_divergent)) "NA" else ps$n_divergent
    ))
  }
  ct <- bundle$fit$convergence$convergence_table %||% bundle$fit$convergence_table
  if ("convergence" %in% types && !is.null(ct)) {
    if (nrow(ct) > 0L) {
      mx <- suppressWarnings(max(ct$rhat, na.rm = TRUE))
      parts <- c(parts, sprintf("Max R-hat (fixed): %.3f.", mx))
    }
  }
  parts
}

#' @rdname ild_diagnose
#' @method ild_diagnose ild_fit_ctsem
#' @export
ild_diagnose.ild_fit_ctsem <- function(object,
                                       data = NULL,
                                       type = NULL,
                                       by_id = NULL,
                                       missing_model = FALSE,
                                       missing_model_predictors = NULL,
                                       causal_detail = FALSE,
                                       balance = FALSE,
                                       balance_treatment = NULL,
                                       balance_covariates = NULL,
                                       balance_weights_col = ".ipw_treat",
                                       balance_by_occasion = FALSE,
                                       ...) {
  rlang::check_installed("ctsem", reason = "to diagnose ild_ctsem fits")
  data <- if (is.null(data)) attr(object, "ild_data", exact = TRUE) else data
  if (is.null(data)) {
    stop("ild_diagnose() for ild_fit_ctsem requires data or attr(fit, \"ild_data\").", call. = FALSE)
  }
  validate_ild(data)
  rn <- attr(object, "ild_response", exact = TRUE)
  outcome_vars <- if (!is.null(rn) && rn %in% names(data)) rn else character()
  fd <- fill_diagnostics_fit_ctsem(object)
  miss <- fill_diagnostics_missingness_section(data, vars = outcome_vars)
  bundle <- ild_diagnostics_bundle(
    meta = list(
      engine = "ctsem",
      n_obs = nrow(data),
      n_id = length(unique(data[[".ild_id"]]))
    ),
    data = fill_diagnostics_data(data, outcome_vars = outcome_vars),
    design = fill_diagnostics_design(data, vars = NULL),
    fit = fd,
    residual = fill_diagnostics_residual_ctsem(object, data),
    predictive = fill_diagnostics_predictive_ctsem(object),
    missingness = miss,
    causal = fill_diagnostics_causal(
      data,
      causal_detail = causal_detail,
      balance = balance,
      balance_treatment = balance_treatment,
      balance_covariates = balance_covariates,
      balance_weights_col = balance_weights_col,
      balance_by_occasion = balance_by_occasion
    ),
    warnings = tibble::tibble(),
    guardrails = guardrails_empty_tibble(),
    summary_text = character()
  )
  bundle$design$occasion_imbalance <- bundle$data$obs_per_id
  bundle$guardrails <- guardrails_bind(
    evaluate_guardrails_fit(object, fd, engine = "ctsem"),
    evaluate_guardrails_contextual(object, data, bundle, engine = "ctsem")
  )
  bundle$summary_text <- build_diagnostics_bundle_summary(bundle)
  bundle <- enrich_bundle_semantic_sections(bundle)
  attr(bundle, "ild_fit") <- object
  attr(bundle, "ild_data") <- data
  bundle
}

#' @rdname ild_diagnose
#' @method ild_diagnose default
#' @export
ild_diagnose.default <- function(object, ...) {
  stop(
    "No ild_diagnose() method for class ",
    paste(class(object), collapse = ", "),
    ". Use lmerMod, lme, brmsfit, ild_fit_kfas, or ild_fit_ctsem.",
    call. = FALSE
  )
}
