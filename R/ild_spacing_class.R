#' Classify spacing as regular-ish vs irregular-ish
#'
#' Returns a simple classification for use in documentation or when choosing
#' correlation structure (e.g. AR1 vs CAR1 in [ild_lme()]). The rule is
#' documented and overridable via arguments. Does not change core ILD behavior.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param cv_threshold Numeric. Coefficient of variation of within-person
#'   intervals above which spacing is "irregular-ish" (default 0.2).
#' @param pct_gap_threshold Numeric. Percent of intervals flagged as gaps
#'   above which spacing is "irregular-ish" (default 10).
#' @return Character: `"regular-ish"` or `"irregular-ish"`.
#' @export
ild_spacing_class <- function(x,
                              cv_threshold = 0.2,
                              pct_gap_threshold = 10) {
  validate_ild(x)
  spacing <- attr(x, "ild_spacing", exact = TRUE)
  dt_median <- spacing$median_dt
  dt_iqr <- spacing$iqr_dt
  pct_gap <- spacing$pct_gap
  if (is.na(dt_median) || dt_median <= 0) return("irregular-ish")
  # CV of interval: use IQR/median as a robust proxy for variability
  cv <- if (is.na(dt_iqr) || dt_iqr == 0) 0 else dt_iqr / dt_median
  if (cv > cv_threshold) return("irregular-ish")
  if (!is.na(pct_gap) && pct_gap > pct_gap_threshold) return("irregular-ish")
  "regular-ish"
}
