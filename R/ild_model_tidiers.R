#' Augment an ILD model fit with fitted values and residuals
#'
#' Returns a tibble with one row per observation: \code{.ild_id}, \code{.ild_time},
#' the response variable (column name from the model formula, e.g. \code{y}),
#' \code{.fitted}, and \code{.resid}. This structure is used internally by
#' [ild_diagnostics()] and [ild_plot()]. Requires \code{attr(fit, "ild_data")};
#' refit with [ild_lme()] if missing. Random effects predictions can be added later.
#'
#' @param fit A fitted model from [ild_lme()] (must have \code{attr(fit, "ild_data")}).
#' @param ... Unused.
#' @return A tibble with columns \code{.ild_id}, \code{.ild_time}, the response
#'   (name from formula), \code{.fitted}, \code{.resid}.
#' @export
augment_ild_model <- function(fit, ...) {
  data <- attr(fit, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop("Fit was not produced by tidyILD; refit using ild_lme() so the fit carries ild_data.",
         call. = FALSE)
  }
  validate_ild(data)
  res <- stats::residuals(fit)
  f <- tryCatch(stats::fitted(fit), error = function(e) NULL)
  if (is.null(f) || length(f) != nrow(data)) f <- rep(NA_real_, nrow(data))
  mf <- tryCatch(stats::model.frame(fit, data = data), error = function(e) stats::model.frame(fit))
  y <- stats::model.response(mf)
  out_name <- ild_response_name(fit)
  out <- tibble::tibble(
    .ild_id = data[[".ild_id"]],
    .ild_time = data[[".ild_time"]],
    .fitted = f,
    .resid = res
  )
  out[[out_name]] <- y
  out <- out[c(".ild_id", ".ild_time", out_name, ".fitted", ".resid")]
  out
}

#' Get response variable name from an ild_lme fit (lmer or lme)
#' @param fit lmerMod or lme object.
#' @return Character; name of the response (e.g. "y"). Falls back to "outcome" if not determined.
#' @noRd
ild_response_name <- function(fit) {
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
#' Returns a tibble of fixed-effect estimates with consistent columns for both
#' lmer and lme engines: \code{term}, \code{estimate}, \code{std_error},
#' \code{ci_low}, \code{ci_high}, \code{p_value}. With \code{object = TRUE},
#' returns an object of class \code{tidyild_model} (meta + table) for use with
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
#' \pkg{clubSandwich} package to use robust SEs.
#'
#' @export
tidy_ild_model <- function(fit, conf_level = 0.95, object = FALSE,
                          se = c("model", "robust"), robust_type = c("CR2", "CR3", "CR0"), ...) {
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
  ci_low <- if (se == "robust" || inherits(fit, "lmerMod")) {
    est + stats::qnorm(q) * se_vec
  } else {
    est + stats::qt(q, df_use) * se_vec
  }
  ci_high <- if (se == "robust" || inherits(fit, "lmerMod")) {
    est + stats::qnorm(1 - q) * se_vec
  } else {
    est + stats::qt(1 - q, df_use) * se_vec
  }
  tbl <- tibble::tibble(
    term = terms,
    estimate = est,
    std_error = se_vec,
    ci_low = ci_low,
    ci_high = ci_high,
    p_value = pval
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
  print(x$table)
  invisible(x)
}
