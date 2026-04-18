# Internal helpers for ild_tidy() schema conformance (ILD_TIDY_REQUIRED_COLS / optional)

#' Conservative effect_level for fixed-effect coefficient rows (term-name based)
#' @keywords internal
#' @noRd
ild_tidy_effect_level_fixed <- function(term) {
  v <- as.character(term)[1]
  if (is.na(v)) return("unknown")
  if (v %in% c("(Intercept)", "Intercept")) return("population")
  if (grepl("_wp$", v)) return("within")
  if (grepl("_bp$", v)) return("between")
  if (grepl(":", v, fixed = TRUE)) {
    if (grepl("_wp", v) && grepl("_bp", v)) return("cross_level")
    return("unknown")
  }
  "unknown"
}

#' @keywords internal
#' @noRd
ild_tidy_model_class_string <- function(fit) {
  paste(class(fit), collapse = ", ")
}

#' Extract test statistic vector aligned to fixed-effect rows
#' @keywords internal
#' @noRd
ild_tidy_extract_statistic <- function(fit, est, se_vec, se_mode) {
  n <- length(est)
  stat <- rep(NA_real_, n)
  if (identical(se_mode, "robust")) {
    ok <- is.finite(est) & is.finite(se_vec) & se_vec > 0
    stat[ok] <- est[ok] / se_vec[ok]
    return(stat)
  }
  if (inherits(fit, "lmerMod")) {
    cc <- stats::coef(summary(fit))
    if (!is.null(cc) && "t value" %in% colnames(cc) && nrow(cc) == n) {
      stat <- as.vector(cc[, "t value"])
    }
  } else if (inherits(fit, "lme")) {
    tt <- summary(fit)$tTable
    if (!is.null(tt) && "t-value" %in% colnames(tt) && nrow(tt) == n) {
      stat <- as.vector(tt[, "t-value"])
    }
  }
  stat
}

#' Interval label for frequentist ild_lme fits
#' @keywords internal
#' @noRd
ild_tidy_interval_type_frequentist <- function(se_mode) {
  if (identical(se_mode, "robust")) return("Wald")
  "Wald"
}

#' Assemble full tidy tibble with required + optional columns (schema order)
#' @keywords internal
#' @noRd
ild_tidy_assemble <- function(term,
                              estimate,
                              std_error,
                              conf_low,
                              conf_high,
                              p_value,
                              statistic,
                              interval_type,
                              engine,
                              model_class,
                              component = "fixed",
                              effect_level = NULL,
                              optional = NULL) {
  if (is.null(effect_level)) {
    effect_level <- vapply(term, ild_tidy_effect_level_fixed, character(1), USE.NAMES = FALSE)
  }
  n <- length(term)
  stopifnot(
    length(estimate) == n, length(std_error) == n, length(conf_low) == n,
    length(conf_high) == n, length(p_value) == n, length(statistic) == n,
    length(interval_type) == n, length(component) == n, length(effect_level) == n
  )
  out <- tibble::tibble(
    term = term,
    component = component,
    effect_level = effect_level,
    estimate = estimate,
    std_error = std_error,
    conf_low = conf_low,
    conf_high = conf_high,
    statistic = statistic,
    p_value = p_value,
    interval_type = interval_type,
    engine = engine,
    model_class = model_class
  )
  opt <- ILD_TIDY_OPTIONAL_COLS
  if (!is.null(optional) && is.list(optional)) {
    for (nm in opt) {
      out[[nm]] <- if (!is.null(optional[[nm]])) optional[[nm]] else rep(NA_real_, n)
    }
  } else {
    for (nm in opt) {
      out[[nm]] <- rep(NA_real_, n)
    }
  }
  out
}
