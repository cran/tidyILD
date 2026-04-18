#' Augment an ILD model fit with fitted values and residuals
#'
#' Returns a tibble conforming to \code{\link{ild_augment_schema}}: \code{.ild_id},
#' \code{.ild_time}, \code{.outcome}, \code{.fitted}, \code{.resid}, \code{.resid_std},
#' \code{engine}, \code{model_class}, plus optional columns (see schema).
#' \code{.resid_std} is Pearson-type when \code{residuals(fit, type = "pearson")} is
#' available and length-matched; otherwise \code{NA} (principled but sparse).
#' Used internally by [ild_diagnostics()] and [ild_plot()]. Requires
#' \code{attr(fit, "ild_data")}; refit with [ild_lme()] if missing.
#'
#' @param fit A fitted model from [ild_lme()] (must have \code{attr(fit, "ild_data")}).
#' @param ... Unused.
#' @return A tibble; see \code{\link{ild_augment_schema}}.
#' @export
augment_ild_model <- function(fit, ...) {
  data <- attr(fit, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop("Fit was not produced by tidyILD; refit using ild_lme() so the fit carries ild_data.",
         call. = FALSE)
  }
  validate_ild(data)
  n <- nrow(data)
  res <- stats::residuals(fit)
  f <- tryCatch(stats::fitted(fit), error = function(e) NULL)
  if (is.null(f) || length(f) != n) f <- rep(NA_real_, n)
  mf <- tryCatch(stats::model.frame(fit, data = data), error = function(e) stats::model.frame(fit))
  y <- stats::model.response(mf)
  y_num <- as.numeric(y)
  if (length(y_num) != n) y_num <- rep(NA_real_, n)
  r_std <- ild_augment_pearson_residuals(fit, n)
  engine <- if (inherits(fit, "lme")) "lme" else "lmer"
  mc <- ild_tidy_model_class_string(fit)
  ild_augment_assemble(
    .ild_id = data[[".ild_id"]],
    .ild_time = data[[".ild_time"]],
    .outcome = y_num,
    .fitted = as.numeric(f),
    .resid = as.numeric(res),
    .resid_std = r_std,
    engine = engine,
    model_class = mc,
    optional = NULL
  )
}

#' Get response variable name from an ild_lme fit (lmer or lme)
#' @param fit lmerMod or lme object.
#' @return Character; name of the response (e.g. "y"). Falls back to "outcome" if not determined.
#' @noRd
ild_response_name <- function(fit) {
  if (inherits(fit, "ild_fit_kfas")) {
    r <- attr(fit, "ild_response", exact = TRUE)
    if (!is.null(r)) return(r)
  }
  if (inherits(fit, "brmsfit")) {
    f <- tryCatch(stats::formula(fit), error = function(e) NULL)
    if (!is.null(f)) {
      vars <- all.vars(f)
      if (length(vars) >= 1L) return(vars[1L])
    }
  }
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f)) return("outcome")
  vars <- all.vars(f)
  if (length(vars) >= 1L) return(vars[1L])
  lhs <- tryCatch(f[[2L]], error = function(e) NULL)
  if (!is.null(lhs)) return(deparse(lhs, nlines = 1L))
  "outcome"
}

#' Tidy fixed effects from an ILD model fit
#'
#' Returns a tibble conforming to \code{\link{ild_tidy_schema}}: required columns include
#' \code{term}, \code{component}, \code{effect_level}, \code{estimate}, \code{std_error},
#' \code{conf_low}, \code{conf_high}, \code{statistic}, \code{p_value}, \code{interval_type},
#' \code{engine}, \code{model_class}; optional Bayesian columns are \code{NA} for these engines.
#' \code{component} is \code{"fixed"} for coefficient rows; \code{effect_level} is inferred
#' conservatively from term names (e.g. \verb{_wp}/\verb{_bp} suffixes) or \code{"population"}
#' for the intercept, \code{"unknown"} when ambiguous.
#'
#' With \code{object = TRUE}, returns an object of class \code{tidyild_model} (meta + table) for use with
#' \code{print.tidyild_model}.
#'
#' @param fit A fitted model from [ild_lme()] (lmerMod or lme).
#' @param conf_level Numeric. Confidence level for intervals (default 0.95).
#' @param object Logical. If \code{TRUE}, return a list with \code{meta} and \code{table}
#'   and class \code{tidyild_model} for polished printing (default \code{FALSE}).
#' @param se Character. \code{"model"} (default) uses the model's standard errors;
#'   \code{"robust"} uses cluster-robust SEs from [ild_robust_se()] (requires
#'   \pkg{clubSandwich}).
#' @param robust_type Character. When \code{se = "robust"}, the correction type:
#'   \code{"CR2"} (recommended), \code{"CR3"}, or \code{"CR0"}.
#' @param ... Passed to [ild_robust_se()] when \code{se = "robust"} (e.g. \code{cluster}, \code{cluster_vec}).
#' @return A tibble, or when \code{object = TRUE} a list of class \code{tidyild_model}.
#'
#' @section Model-based vs robust SE:
#' With \code{se = "model"} (default), standard errors and CIs come from the fitted
#' model. With \code{se = "robust"}, cluster-robust (sandwich) SEs are used;
#' CIs and p-values are based on a Wald normal approximation. Install the
#' \pkg{clubSandwich} package to use robust SEs. For **IPW / MSM** weighted \code{lmer}
#' fits ([ild_ipw_refit()]), robust SEs do not account for estimated weights; use
#' [ild_msm_bootstrap()] and [tidy_ild_msm_bootstrap()] when you need bootstrap CIs;
#' see [ild_msm_inference].
#'
#' @export
tidy_ild_model <- function(fit, conf_level = 0.95, object = FALSE,
                          se = c("model", "robust"), robust_type = c("CR2", "CR3", "CR0"), ...) {
  if (inherits(fit, "ild_msm_bootstrap")) {
    stop("Use tidy_ild_msm_bootstrap() for objects from ild_msm_bootstrap(); tidy_ild_model() expects a model fit.",
         call. = FALSE)
  }
  se <- match.arg(se)
  robust_type <- match.arg(robust_type)
  q <- (1 - conf_level) / 2
  engine <- if (inherits(fit, "lme")) "lme" else "lmer"
  ar1 <- isTRUE(attr(fit, "ild_ar1", exact = TRUE))
  if (inherits(fit, "lme")) {
    tt <- summary(fit)$tTable
    if (is.null(tt)) stop("Could not extract fixed-effect table from lme fit.", call. = FALSE)
    est <- as.vector(tt[, "Value"])
    se_vec <- as.vector(tt[, "Std.Error"])
    pval <- as.vector(tt[, "p-value"])
    terms <- rownames(tt)
    df_use <- tt[, "DF"]
  } else if (inherits(fit, "lmerMod")) {
    cc <- summary(fit)$coefficients
    if (is.null(cc)) stop("Could not extract coefficient table from lmer fit.", call. = FALSE)
    est <- as.vector(cc[, "Estimate"])
    se_vec <- as.vector(cc[, "Std. Error"])
    pval <- if ("Pr(>|t|)" %in% colnames(cc)) {
      as.vector(cc[, "Pr(>|t|)"])
    } else {
      rep(NA_real_, length(est))
    }
    terms <- rownames(cc)
    df_use <- NULL
  } else {
    stop("tidy_ild_model() supports only fits from ild_lme() (lme or lmerMod).", call. = FALSE)
  }
  if (se == "robust") {
    rlang::check_installed("clubSandwich", reason = "to use robust standard errors (se = \"robust\")")
    rob <- ild_robust_se(fit, type = robust_type, ...)
    vc <- rob$vcov
    if (nrow(vc) != length(est) || ncol(vc) != length(est)) {
      stop("Robust vcov dimension does not match fixed-effect length.", call. = FALSE)
    }
    se_vec <- sqrt(diag(vc))
    pval <- 2 * (1 - stats::pnorm(abs(est / se_vec)))
  }
  conf_low <- if (se == "robust" || inherits(fit, "lmerMod")) {
    est + stats::qnorm(q) * se_vec
  } else {
    est + stats::qt(q, df_use) * se_vec
  }
  conf_high <- if (se == "robust" || inherits(fit, "lmerMod")) {
    est + stats::qnorm(1 - q) * se_vec
  } else {
    est + stats::qt(1 - q, df_use) * se_vec
  }
  stat_vec <- ild_tidy_extract_statistic(fit, est, se_vec, se_mode = se)
  int_type <- rep(ild_tidy_interval_type_frequentist(se), length(est))
  mc <- ild_tidy_model_class_string(fit)
  tbl <- ild_tidy_assemble(
    term = terms,
    estimate = est,
    std_error = se_vec,
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = pval,
    statistic = stat_vec,
    interval_type = int_type,
    engine = engine,
    model_class = mc,
    component = rep("fixed", length(terms)),
    effect_level = NULL,
    optional = NULL
  )
  if (!object) return(tbl)
  out <- list(
    meta = list(engine = engine, ar1 = ar1, se = se),
    table = tbl
  )
  class(out) <- "tidyild_model"
  out
}

#' @export
print.tidyild_model <- function(x, ...) {
  se_info <- if (identical(x$meta$se, "robust")) " robust SE" else ""
  cat("Fixed effects (", x$meta$engine, if (x$meta$ar1) ", AR1/CAR1" else "", se_info, ")\n", sep = "")
  show <- x$table
  keep <- intersect(
    c("term", "estimate", "std_error", "conf_low", "conf_high", "component", "effect_level", "p_value"),
    names(show)
  )
  print(show[, keep, drop = FALSE])
  invisible(x)
}
