#' Spacing diagnostics and correlation-structure recommendation
#'
#' Reports observation intervals in human-friendly units (e.g. hours) and
#' recommends AR1 vs CAR1 for use in [ild_lme()]. Surfaces the same logic
#' that \code{ild_lme(..., ar1 = TRUE)} uses internally so users can see
#' why a correlation structure was chosen.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param gap_large_hours Numeric. Intervals (in hours) above which to count
#'   as "large gaps" for \code{large_gaps_pct} (default 12). Ignored if time
#'   is not in seconds (e.g. numeric day indices).
#' @return A list with \code{median_interval} (hours), \code{iqr} (hours),
#'   \code{large_gaps_pct} (percent of intervals \code{> gap_large_hours}),
#'   \code{coefficient_of_variation}, \code{recommendation} (character: use CAR1 or AR1),
#'   and \code{spacing_class} (\code{regular-ish} or \code{irregular-ish}).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 5, n_obs_per = 10, irregular = TRUE, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
#' ild_spacing(x)
ild_spacing <- function(x, gap_large_hours = 12) {
  validate_ild(x)
  meta <- ild_meta(x)
  spacing <- meta$ild_spacing
  dt <- x[[".ild_dt"]]
  dt <- dt[!is.na(dt)]
  if (length(dt) == 0) {
    return(list(
      median_interval = NA_real_,
      iqr = NA_real_,
      large_gaps_pct = NA_real_,
      coefficient_of_variation = NA_real_,
      recommendation = "Insufficient intervals; cannot recommend.",
      spacing_class = NA_character_
    ))
  }
  median_dt_sec <- if (!is.null(spacing$median_dt)) spacing$median_dt else stats::median(dt)
  iqr_dt_sec <- if (!is.null(spacing$iqr_dt)) spacing$iqr_dt else stats::IQR(dt)
  # Assume .ild_time_num is in seconds (POSIXct); convert to hours for display
  sec_per_hour <- 3600
  median_interval_h <- median_dt_sec / sec_per_hour
  iqr_h <- iqr_dt_sec / sec_per_hour
  # CV: use IQR/median as robust proxy (consistent with ild_spacing_class)
  cv <- if (median_dt_sec <= 0 || is.na(median_dt_sec)) NA_real_ else {
    if (is.na(iqr_dt_sec) || iqr_dt_sec == 0) 0 else iqr_dt_sec / median_dt_sec
  }
  gap_large_sec <- gap_large_hours * sec_per_hour
  large_gaps_pct <- mean(dt > gap_large_sec) * 100
  spacing_class <- ild_spacing_class(x)
  recommendation <- if (spacing_class == "irregular-ish") {
    "Use CAR1 correlation structure."
  } else {
    "Use AR1 correlation structure."
  }
  list(
    median_interval = median_interval_h,
    iqr = iqr_h,
    large_gaps_pct = large_gaps_pct,
    coefficient_of_variation = cv,
    recommendation = recommendation,
    spacing_class = spacing_class
  )
}
