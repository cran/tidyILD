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
#' @param warn_uncentered If `TRUE` (default), warn when a predictor in the
#'   formula varies both within and between persons but is not decomposed
#'   (no \code{_wp}/\code{_bp}); suggests using [ild_center()].
#' @param ... Passed to [lme4::lmer()] or [nlme::lme()].
#' @return A fitted model object (class `lmerMod` or `lme`) with attribute
#'   `ild_data` (the ILD data) and `ild_ar1` (logical). When `ar1 = TRUE`,
#'   the returned object has class `ild_lme` prepended and attribute
#'   `ild_random_resolved` (the formula actually passed to nlme, e.g. `~ 1 | M2ID`).
#'   See [ild_diagnostics()] and [ild_plot()].
#' @examples
#' # lme4 path: formula includes random effects
#' set.seed(1)
#' dat <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
#' dat <- ild_prepare(dat, id = "id", time = "time")
#' dat <- ild_center(dat, y)
#' fit_lmer <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = dat,
#'                     ar1 = FALSE, warn_no_ar1 = FALSE)
#' # nlme path (may not converge on all platforms; see ?nlme::lme)
#' \dontrun{
#' fit_lme <- ild_lme(y ~ y_bp + y_wp, data = dat,
#'                    random = ~ 1 | id, ar1 = TRUE)
#' }
#'
#' @importFrom lme4 lmer
#' @importFrom nlme corAR1 corCAR1 lme
#' @export
ild_lme <- function(formula,
                    data,
                    ar1 = FALSE,
                    correlation_class = c("auto", "AR1", "CAR1"),
                    random = ~ 1 | .ild_id,
                    warn_no_ar1 = TRUE,
                    warn_uncentered = TRUE,
                    ...) {
  validate_ild(data)
  if (warn_uncentered) {
    uncentered <- ild_detect_uncentered_predictors(data, formula)
    if (length(uncentered) > 0) {
      warning("Predictor(s) '", paste(uncentered, collapse = "', '"),
              "' vary both within and between persons. Consider using ild_center(",
              paste(uncentered, collapse = ", "), ") to separate within- and ",
              "between-person effects and avoid conflation bias.", call. = FALSE)
    }
  }
  correlation_class <- match.arg(correlation_class)
  if (ar1) {
    if (ild_formula_has_random(formula)) {
      stop("When ar1 = TRUE, use a fixed-effects-only formula and pass random ",
           "effects via the random argument. Formula must not contain (1|id) or similar. ",
           "In model formulas, | is interpreted as an lme4-style random-effects operator.",
           call. = FALSE)
    }
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
    attr(fit, "ild_data") <- data  # both engines set ild_data for augment_ild_model/diagnostics
    attr(fit, "ild_ar1") <- TRUE
    attr(fit, "ild_correlation_class") <- cor_class
    attr(fit, "ild_random_resolved") <- random_form
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
  attr(fit, "ild_data") <- data  # both engines set ild_data for augment_ild_model/diagnostics
  attr(fit, "ild_ar1") <- FALSE
  # Do not add ild_lme to class for S4 lmerMod (breaks residuals/fitted dispatch)
  fit
}

#' Detect if formula contains random effects (e.g. (1|id))
#' @param f Formula or call to recurse.
#' @return Logical.
#' @noRd
ild_formula_has_random <- function(f) {
  if (is.call(f)) {
    if (identical(f[[1]], as.name("|"))) return(TRUE)
    for (i in seq_along(f)) {
      if (i == 1) next
      if (ild_formula_has_random(f[[i]])) return(TRUE)
    }
  }
  FALSE
}

#' Predictors that vary both WP and BP but are not _wp/_bp (uncentered).
#' @param data ILD object
#' @param formula Model formula (fixed or mixed)
#' @return Character vector of variable names to warn about (may be empty).
#' @noRd
ild_detect_uncentered_predictors <- function(data, formula) {
  all_v <- all.vars(formula)
  if (length(all_v) <= 1) return(character())
  preds <- all_v[-1]
  nms <- names(data)
  id_col <- ".ild_id"
  out <- character()
  for (v in preds) {
    if (!v %in% nms) next
    if (!is.numeric(data[[v]])) next
    if (grepl("_wp$|_bp$", v)) next
    means_i <- tapply(data[[v]], data[[id_col]], mean, na.rm = TRUE)
    bp_var <- stats::var(as.vector(means_i))
    if (is.na(bp_var) || bp_var < 1e-10) next
    wp_var_by_id <- tapply(data[[v]], data[[id_col]], function(z) stats::var(z, na.rm = TRUE))
    wp_var_by_id[is.na(wp_var_by_id)] <- 0
    wp_var <- mean(wp_var_by_id, na.rm = TRUE)
    if (is.na(wp_var) || wp_var < 1e-10) next
    out <- c(out, v)
  }
  unique(out)
}
