#' Fit a linear mixed-effects model to ILD
#'
#' When `ar1 = FALSE`, fits with [lme4::lmer()] (no residual correlation).
#' When `ar1 = TRUE`, fits with [nlme::lme()] using a residual correlation
#' structure: CAR1 (continuous-time) by default for irregular spacing,
#' or AR1 when spacing is regular-ish. Use [ild_spacing_class()] to inform
#' the choice; override with `correlation_class`.
#'
#' @param formula Fixed-effects formula. For `ar1 = TRUE`, must be fixed-only
#'   (e.g. `y ~ x`); random structure is set to `~ 1 | .ild_id` internally.
#'   For `ar1 = FALSE`, formula may include random effects (e.g. `y ~ x + (1|id)`).
#' @param data An ILD object (see [is_ild()]).
#' @param ar1 Logical. If `TRUE`, fit with nlme and residual AR1/CAR1
#'   correlation; if `FALSE`, fit with lme4 (no residual correlation).
#' @param correlation_class Character. `"auto"` (default) uses [ild_spacing_class()]
#'   to choose CAR1 (irregular-ish) or AR1 (regular-ish). Use `"CAR1"` or
#'   `"AR1"` to override.
#' @param random For `ar1 = TRUE`, the random effects formula (default
#'   `~ 1 | .ild_id`). Must use `.ild_id` as grouping for correlation to match.
#' @param warn_no_ar1 If `TRUE` (default), warn when `ar1 = FALSE` that
#'   temporal autocorrelation is not modeled.
#' @param ... Passed to [lme4::lmer()] or [nlme::lme()].
#' @return A fitted model object (class `lmerMod` or `lme`) with attribute
#'   `ild_data` (the ILD data) and `ild_ar1` (logical). When `ar1 = TRUE`,
#'   the returned object has class `ild_lme` prepended for [ild_diagnostics()]
#'   and [ild_plot()].
#' @importFrom lme4 lmer
#' @importFrom nlme corAR1 corCAR1 lme
#' @export
ild_lme <- function(formula,
                    data,
                    ar1 = FALSE,
                    correlation_class = c("auto", "AR1", "CAR1"),
                    random = ~ 1 | .ild_id,
                    warn_no_ar1 = TRUE,
                    ...) {
  validate_ild(data)
  correlation_class <- match.arg(correlation_class)
  if (ar1) {
    if (!requireNamespace("nlme", quietly = TRUE)) {
      stop("Package 'nlme' is required for ar1 = TRUE.", call. = FALSE)
    }
    meta <- ild_meta(data)
    id_name <- meta$ild_id
    cor_class <- if (correlation_class == "auto") {
      if (ild_spacing_class(data) == "regular-ish") "AR1" else "CAR1"
    } else {
      correlation_class
    }
    cor_form <- if (cor_class == "CAR1") {
      nlme::corCAR1(form = stats::as.formula(paste0("~ .ild_time_num | ", id_name)))
    } else {
      nlme::corAR1(form = stats::as.formula(paste0("~ .ild_seq | ", id_name)))
    }
    random_form <- if (identical(random, ~ 1 | .ild_id)) {
      stats::as.formula(paste0("~ 1 | ", id_name))
    } else {
      random
    }
    fit <- nlme::lme(
      fixed = formula,
      data = data,
      random = random_form,
      correlation = cor_form,
      ...
    )
    attr(fit, "ild_data") <- data
    attr(fit, "ild_ar1") <- TRUE
    attr(fit, "ild_correlation_class") <- cor_class
    class(fit) <- c(class(fit), "ild_lme")
    return(fit)
  }
  if (warn_no_ar1) {
    message("Temporal autocorrelation is not modeled (ar1 = FALSE). Consider ar1 = TRUE for ILD.")
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for ar1 = FALSE.", call. = FALSE)
  }
  fit <- lme4::lmer(formula, data = data, ...)
  attr(fit, "ild_data") <- data
  attr(fit, "ild_ar1") <- FALSE
  # Do not add ild_lme to class for S4 lmerMod (breaks residuals/fitted dispatch)
  fit
}
