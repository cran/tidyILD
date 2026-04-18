# MSM estimand-first runner (v1.1 scaffold)

#' Fit an MSM-style weighted mixed model from an estimand spec
#'
#' High-level runner that takes an [ild_msm_estimand()] plus ILD data, builds
#' treatment (and optional censoring) weights, refits a weighted \code{lmer},
#' and optionally computes inference via robust SEs or cluster bootstrap.
#'
#' v1.1 keeps the v1 workflow and adds explicit capability signaling for
#' inference/regime paths that are degraded or unsupported.
#'
#' @param estimand Object from [ild_msm_estimand()].
#' @param data ILD data.
#' @param outcome_formula Outcome model formula for [ild_lme()] (typically includes treatment term).
#' @param history Predictors for treatment process weights:
#'   one-sided formula (e.g. \code{~ stress_lag1 + trt_lag1}) or character vector.
#' @param history_spec Optional [ild_msm_history_spec()] to build lagged history before weighting.
#' @param treatment_engine \code{"sequential_msm"} (default) or \code{"pooled"}.
#' @param predictors_censor Optional character vector for IPCW model. If supplied, joint weights
#'   are built with [ild_joint_msm_weights()] and stored in \code{weights_col}.
#' @param weights_col Column used by [ild_ipw_refit()] (default \code{".ipw"}).
#' @param stabilize_treat Treatment-weight stabilization mode. For sequential MSM:
#'   \code{"marginal"}, \code{"prior_treatment"}, \code{"none"}. For pooled IPTW:
#'   mapped to logical \code{stabilize}.
#' @param prior_treatment Required when \code{stabilize_treat = "prior_treatment"} in sequential MSM.
#' @param stabilize_joint Joint-weight stabilization for [ild_joint_msm_weights()].
#' @param trim Quantile trimming applied during weight construction.
#' @param inference \code{"none"} (default), \code{"robust"}, or \code{"bootstrap"}.
#' @param robust_type Passed to [ild_robust_se()] and [tidy_ild_model()] when \code{inference = "robust"}.
#' @param n_boot Passed to [ild_msm_bootstrap()] when \code{inference = "bootstrap"}.
#' @param weight_policy Passed to [ild_msm_bootstrap()] when \code{inference = "bootstrap"}.
#' @param weights_fn Passed to [ild_msm_bootstrap()] when \code{inference = "bootstrap"} and
#'   \code{weight_policy = "reestimate_weights"}.
#' @param seed Optional seed for bootstrap inference.
#' @param strict_inference Logical. If \code{TRUE}, stop when requested inference or
#'   regime dispatch cannot be delivered as requested. If \code{FALSE} (default),
#'   degrade with warning and record machine-readable status.
#' @param ... Passed to [ild_lme()].
#' @return Object of class \code{ild_msm_fit} with elements:
#'   \code{estimand}, \code{history_spec}, \code{weights_data}, \code{fit}, \code{inference},
#'   \code{treatment_engine}, \code{weights_col}. Analysis provenance is attached.
#' @export
#' @examples
#' set.seed(31)
#' d <- ild_simulate(n_id = 12, n_obs_per = 6, seed = 31)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
#' d <- ild_prepare(d, id = "id", time = "time")
#' d <- ild_center(d, y)
#' d <- ild_lag(d, stress, max_gap = Inf)
#' d <- ild_lag(d, trt, max_gap = Inf)
#' est <- ild_msm_estimand(treatment = "trt")
#' res <- ild_msm_fit(
#'   estimand = est,
#'   data = d,
#'   outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
#'   history = ~ stress_lag1 + trt_lag1,
#'   predictors_censor = "stress",
#'   inference = "none",
#'   warn_no_ar1 = FALSE,
#'   warn_uncentered = FALSE
#' )
#' res$fit
ild_msm_fit <- function(estimand,
                        data,
                        outcome_formula,
                        history,
                        history_spec = NULL,
                        treatment_engine = c("sequential_msm", "pooled"),
                        predictors_censor = NULL,
                        weights_col = ".ipw",
                        stabilize_treat = c("marginal", "prior_treatment", "none"),
                        prior_treatment = NULL,
                        stabilize_joint = c("mean1", "none"),
                        trim = c(0.01, 0.99),
                        inference = c("none", "robust", "bootstrap"),
                        robust_type = c("CR2", "CR3", "CR0"),
                        n_boot = 200L,
                        weight_policy = c("fixed_weights", "reestimate_weights"),
                        weights_fn = NULL,
                        seed = NULL,
                        strict_inference = FALSE,
                        ...) {
  validate_ild_msm_estimand(estimand)
  validate_ild(data)
  treatment_engine <- match.arg(treatment_engine)
  stabilize_treat <- match.arg(stabilize_treat)
  stabilize_joint <- match.arg(stabilize_joint)
  inference <- match.arg(inference)
  robust_type <- match.arg(robust_type)
  weight_policy <- match.arg(weight_policy)
  strict_inference <- isTRUE(strict_inference)

  if (!estimand$treatment %in% names(data)) {
    stop("Treatment column '", estimand$treatment, "' not found in data.", call. = FALSE)
  }
  if (!estimand$time_var %in% names(data)) {
    stop("time_var '", estimand$time_var, "' not found in data.", call. = FALSE)
  }
  if (missing(outcome_formula)) {
    stop("outcome_formula is required.", call. = FALSE)
  }
  if (missing(history)) {
    stop("history is required (formula or character vector).", call. = FALSE)
  }

  x <- data
  regime_status <- list(
    status = "ok",
    reason = "none",
    message = "Regime dispatch completed."
  )
  if (identical(estimand$regime, "dynamic")) {
    x$.msm_regime_assignment <- .ild_msm_eval_dynamic_rule(x, estimand)
    regime_status <- list(
      status = "degraded",
      reason = "dynamic_regime_weighting_not_implemented",
      message = "Dynamic regime assignment was evaluated, but weighting still follows observed treatment in v1.1."
    )
    if (strict_inference) {
      stop(regime_status$message, call. = FALSE)
    }
    warning(regime_status$message, call. = FALSE)
  }
  if (!identical(estimand$type, "ate")) {
    msg_type <- "Estimand type is accepted by schema but not fully implemented in runner; proceeding with ATE-compatible pathway."
    if (strict_inference) stop(msg_type, call. = FALSE)
    warning(msg_type, call. = FALSE)
  }
  if (!is.null(history_spec)) {
    x <- ild_build_msm_history(x, history_spec)
  }

  if (treatment_engine == "sequential_msm") {
    x <- ild_iptw_msm_weights(
      x,
      treatment = estimand$treatment,
      history = history,
      time_var = estimand$time_var,
      stabilize = stabilize_treat,
      prior_treatment = prior_treatment,
      trim = trim
    )
  } else {
    pred <- .ild_history_to_predictors(history)
    if (length(pred) == 0L) {
      stop("Pooled IPTW requires at least one predictor in history.", call. = FALSE)
    }
    x <- ild_iptw_weights(
      x,
      treatment = estimand$treatment,
      predictors = pred,
      stabilize = !identical(stabilize_treat, "none"),
      trim = trim
    )
  }

  if (!is.null(predictors_censor) && length(predictors_censor) > 0L) {
    x <- ild_ipcw_weights(x, predictors = predictors_censor, trim = trim)
    x <- ild_joint_msm_weights(x, stabilize = stabilize_joint, trim = trim, joint_name = weights_col)
  } else {
    if (!".ipw_treat" %in% names(x)) stop("Expected .ipw_treat after treatment weighting.", call. = FALSE)
    attrs <- attributes(x)
    x[[weights_col]] <- x$.ipw_treat
    x <- .ild_ipw_restore_attrs(x, attrs)
  }

  base_fit <- ild_lme(outcome_formula, data = x, ar1 = FALSE, ...)
  fitw <- ild_ipw_refit(base_fit, data = x, weights = weights_col)

  inf <- .ild_msm_inference_shell(
    requested = inference,
    strict_inference = strict_inference
  )
  if (inference == "robust") {
    rb_try <- tryCatch(
      {
        vc <- ild_robust_se(fitw, type = robust_type)
        td <- tidy_ild_model(fitw, se = "robust", robust_type = robust_type)
        list(ok = TRUE, vcov = vc, tidy = td, error = NULL)
      },
      error = function(e) list(ok = FALSE, vcov = NULL, tidy = NULL, error = e)
    )
    if (isTRUE(rb_try$ok)) {
      inf$status <- "ok"
      inf$reason <- "none"
      inf$message <- "Robust cluster-robust inference delivered."
      inf$method <- "robust"
      inf$raw$robust <- list(vcov = rb_try$vcov, tidy = rb_try$tidy)
      inf$summary <- .ild_msm_inference_summary(rb_try$tidy, method = "robust")
    } else {
      msg <- paste0(
        "Robust inference unavailable for this weighted lmer path: ",
        conditionMessage(rb_try$error),
        ". Falling back to model-based Wald intervals."
      )
      if (strict_inference) stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      td <- tidy_ild_model(fitw, se = "model")
      inf$status <- "degraded"
      inf$reason <- "robust_weighted_lmer_not_supported"
      inf$message <- msg
      inf$method <- "model_fallback"
      inf$raw$robust <- list(vcov = NULL, tidy = NULL, error = conditionMessage(rb_try$error))
      inf$summary <- .ild_msm_inference_summary(td, method = "model_fallback")
    }
  } else if (inference == "bootstrap") {
    bres <- ild_msm_bootstrap(
      fit = fitw,
      n_boot = n_boot,
      weight_policy = weight_policy,
      weights_fn = weights_fn,
      seed = seed
    )
    inf$method <- "bootstrap"
    inf$raw$bootstrap <- list(result = bres)
    inf$summary <- .ild_msm_inference_summary(tidy_ild_msm_bootstrap(bres), method = "bootstrap")
    if (is.null(bres$n_success) || bres$n_success <= 0L) {
      msg <- "Bootstrap inference did not produce any successful replicates."
      if (strict_inference) stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      inf$status <- "unsupported"
      inf$reason <- "bootstrap_no_successful_replicates"
      inf$message <- msg
    } else if (bres$n_success < bres$n_boot) {
      msg <- paste0(
        "Bootstrap inference delivered with partial failures (",
        bres$n_success, "/", bres$n_boot, " successful replicates)."
      )
      if (strict_inference) stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      inf$status <- "degraded"
      inf$reason <- "bootstrap_partial_failure"
      inf$message <- msg
    } else {
      inf$status <- "ok"
      inf$reason <- "none"
      inf$message <- "Bootstrap inference delivered."
    }
  } else {
    inf$status <- "ok"
    inf$reason <- "none"
    inf$message <- "Inference not requested."
    inf$method <- "none"
    inf$summary <- tibble::tibble(
      term = character(),
      terms = character(),
      estimate = numeric(),
      std_error = numeric(),
      conf_low = numeric(),
      conf_high = numeric(),
      interval_type = character(),
      method = character()
    )
  }

  out <- list(
    estimand = estimand,
    history_spec = history_spec,
    weights_data = x,
    fit = fitw,
    inference = inf,
    regime_status = regime_status,
    treatment_engine = treatment_engine,
    weights_col = weights_col
  )
  class(out) <- c("ild_msm_fit", "list")
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(
    fitw,
    "ild_msm_fit",
    list(
      estimand = estimand,
      treatment_engine = treatment_engine,
      weights_col = weights_col,
      inference = inference,
      inference_status = inf$status,
      inference_reason = inf$reason,
      regime_status = regime_status$status,
      regime_reason = regime_status$reason,
      strict_inference = strict_inference
    ),
    list(n_obs = nrow(x), n_id = length(unique(x[[".ild_id"]])))
  )
  out
}

#' @export
print.ild_msm_fit <- function(x, ...) {
  cat("tidyILD MSM estimand fit\n")
  cat("  estimand: ", x$estimand$type, " (", x$estimand$regime, ")\n", sep = "")
  cat("  treatment: ", x$estimand$treatment, "\n", sep = "")
  cat("  treatment_engine: ", x$treatment_engine, "\n", sep = "")
  cat("  weights_col: ", x$weights_col, "\n", sep = "")
  if (!is.null(x$inference)) {
    cat("  inference: ", x$inference$method, " [", x$inference$status, "]\n", sep = "")
  }
  if (!is.null(x$regime_status)) {
    cat("  regime_status: ", x$regime_status$status, "\n", sep = "")
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
.ild_history_to_predictors <- function(history) {
  if (inherits(history, "formula")) {
    return(tryCatch(attr(stats::terms(history), "term.labels"), error = function(e) character(0)))
  }
  as.character(history)
}

#' Diagnose an \code{ild_msm_fit} result in one call
#'
#' Convenience bridge to [ild_diagnose()] using \code{object$fit} and
#' \code{object$weights_data}.
#'
#' @param object Output from [ild_msm_fit()].
#' @param ... Passed to [ild_diagnose()].
#' @return An \code{\link{ild_diagnostics_bundle}}.
#' @export
ild_msm_diagnose <- function(object, ...) {
  if (!inherits(object, "ild_msm_fit")) {
    stop("object must be from ild_msm_fit().", call. = FALSE)
  }
  ild_diagnose(object$fit, data = object$weights_data, ...)
}

#' @keywords internal
#' @noRd
.ild_msm_inference_shell <- function(requested, strict_inference) {
  list(
    requested = requested,
    strict_inference = strict_inference,
    status = "unsupported",
    reason = "pending",
    message = "Inference path not evaluated.",
    method = requested,
    summary = NULL,
    raw = list()
  )
}

#' @keywords internal
#' @noRd
.ild_msm_inference_summary <- function(tidy_tbl, method) {
  req <- c("term", "estimate", "std_error", "conf_low", "conf_high", "interval_type")
  miss <- setdiff(req, names(tidy_tbl))
  if (length(miss) > 0L) {
    stop("Inference tidy table missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  out <- tidy_tbl[, req, drop = FALSE]
  out$terms <- out$term
  out$method <- method
  tibble::as_tibble(out)[, c("term", "terms", "estimate", "std_error", "conf_low", "conf_high", "interval_type", "method")]
}

#' @keywords internal
#' @noRd
.ild_msm_eval_dynamic_rule <- function(data, estimand) {
  rs <- estimand$regime_spec
  if (is.null(rs) || !identical(rs$class, "dynamic")) {
    stop("estimand$regime_spec must describe a dynamic regime.", call. = FALSE)
  }
  rule <- rs$rule
  trt <- estimand$treatment
  out <- NULL
  if (is.character(rule)) {
    if (identical(rule, "always_treat")) out <- rep(1L, nrow(data))
    if (identical(rule, "never_treat")) out <- rep(0L, nrow(data))
    if (identical(rule, "as_observed")) out <- as.integer(data[[trt]])
  } else if (is.function(rule)) {
    out <- rule(data)
  }
  if (is.null(out) || length(out) != nrow(data)) {
    stop("Dynamic regime rule must return length nrow(data).", call. = FALSE)
  }
  out <- as.integer(out)
  bad <- !is.finite(out) | !out %in% c(0L, 1L)
  if (any(bad)) {
    stop("Dynamic regime assignment must be binary 0/1 for all rows.", call. = FALSE)
  }
  out
}
