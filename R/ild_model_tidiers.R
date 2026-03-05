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
#' @param ... Unused.
#' @return A tibble, or when \code{object = TRUE} a list of class \code{tidyild_model}.
#' @export
tidy_ild_model <- function(fit, conf_level = 0.95, object = FALSE, ...) {
  q <- (1 - conf_level) / 2
  engine <- if (inherits(fit, "lme")) "lme" else "lmer"
  ar1 <- isTRUE(attr(fit, "ild_ar1", exact = TRUE))
  if (inherits(fit, "lme")) {
    tt <- summary(fit)$tTable
    if (is.null(tt)) stop("Could not extract fixed-effect table from lme fit.", call. = FALSE)
    tbl <- tibble::tibble(
      term = rownames(tt),
      estimate = as.vector(tt[, "Value"]),
      std_error = as.vector(tt[, "Std.Error"]),
      ci_low = as.vector(tt[, "Value"]) + stats::qt(q, tt[, "DF"]) * as.vector(tt[, "Std.Error"]),
      ci_high = as.vector(tt[, "Value"]) + stats::qt(1 - q, tt[, "DF"]) * as.vector(tt[, "Std.Error"]),
      p_value = as.vector(tt[, "p-value"])
    )
  } else if (inherits(fit, "lmerMod")) {
    cc <- summary(fit)$coefficients
    if (is.null(cc)) stop("Could not extract coefficient table from lmer fit.", call. = FALSE)
    est <- as.vector(cc[, "Estimate"])
    se <- as.vector(cc[, "Std. Error"])
    pval <- if ("Pr(>|t|)" %in% colnames(cc)) {
      as.vector(cc[, "Pr(>|t|)"])
    } else {
      rep(NA_real_, length(est))
    }
    tbl <- tibble::tibble(
      term = rownames(cc),
      estimate = est,
      std_error = se,
      ci_low = est + stats::qnorm(q) * se,
      ci_high = est + stats::qnorm(1 - q) * se,
      p_value = pval
    )
  } else {
    stop("tidy_ild_model() supports only fits from ild_lme() (lme or lmerMod).", call. = FALSE)
  }
  if (!object) return(tbl)
  out <- list(
    meta = list(engine = engine, ar1 = ar1),
    table = tbl
  )
  class(out) <- "tidyild_model"
  out
}

#' @export
print.tidyild_model <- function(x, ...) {
  cat("Fixed effects (", x$meta$engine, if (x$meta$ar1) ", AR1/CAR1" else "", ")\n", sep = "")
  print(x$table)
  invisible(x)
}
