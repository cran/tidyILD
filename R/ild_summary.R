#' One-shot summary of an ILD object
#'
#' Reports number of persons, number of observations, time range,
#' descriptive spacing (median/IQR of intervals, percent gaps), and
#' duplicate info. Uses [ild_meta()] and `.ild_*` columns only.
#' No hard "regular"/"irregular" label; use [ild_spacing_class()] for that.
#'
#' @param x An ILD object (see [is_ild()]).
#' @return A list with elements: \code{summary} (one-row tibble with n_id, n_obs,
#'   time_min, time_max, prop_gap, median_dt_sec, iqr_dt_sec), \code{n_units},
#'   \code{n_obs}, \code{time_range}, \code{spacing}, \code{n_gaps}, \code{pct_gap}.
#'   The \code{summary} tibble is the primary contract for programmatic use.
#' @importFrom tibble tibble
#' @export
ild_summary <- function(x) {
  validate_ild(x)
  meta <- ild_meta(x)
  time_num <- x[[".ild_time_num"]]
  n_gaps <- sum(x[[".ild_gap"]] %in% TRUE, na.rm = TRUE)
  n_units <- meta$ild_n_units
  n_obs <- meta$ild_n_obs
  time_range <- range(time_num, na.rm = TRUE)
  spacing <- meta$ild_spacing
  n_intervals <- if (!is.null(spacing$n_intervals)) spacing$n_intervals else sum(!is.na(x[[".ild_dt"]]))
  prop_gap <- if (n_intervals > 0) n_gaps / n_intervals else NA_real_
  median_dt_sec <- if (!is.null(spacing$median_dt)) spacing$median_dt else stats::median(x[[".ild_dt"]], na.rm = TRUE)
  iqr_dt_sec <- if (!is.null(spacing$iqr_dt)) spacing$iqr_dt else stats::IQR(x[[".ild_dt"]], na.rm = TRUE)
  summary_tbl <- tibble::tibble(
    n_id = n_units,
    n_obs = n_obs,
    time_min = time_range[1],
    time_max = time_range[2],
    prop_gap = prop_gap,
    median_dt_sec = median_dt_sec,
    iqr_dt_sec = iqr_dt_sec
  )
  list(
    summary = summary_tbl,
    n_units = n_units,
    n_obs = n_obs,
    time_range = time_range,
    spacing = spacing,
    n_gaps = n_gaps,
    pct_gap = meta$ild_spacing$pct_gap
  )
}
