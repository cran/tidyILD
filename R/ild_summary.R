#' One-shot summary of an ILD object
#'
#' Reports number of persons, number of observations, time range,
#' descriptive spacing (median/IQR of intervals, percent gaps), and
#' duplicate info. Uses [ild_meta()] and `.ild_*` columns only.
#' No hard "regular"/"irregular" label; use [ild_spacing_class()] for that.
#'
#' @param x An ILD object (see [is_ild()]).
#' @return A list with elements: `n_units`, `n_obs`, `time_range` (min/max of
#'   `.ild_time_num`), `spacing` (from metadata), `n_gaps` (sum of `.ild_gap`
#'   TRUE), `pct_gap` (from spacing if available).
#' @export
ild_summary <- function(x) {
  validate_ild(x)
  meta <- ild_meta(x)
  time_num <- x[[".ild_time_num"]]
  n_gaps <- sum(x[[".ild_gap"]] %in% TRUE, na.rm = TRUE)
  list(
    n_units    = meta$ild_n_units,
    n_obs      = meta$ild_n_obs,
    time_range = range(time_num, na.rm = TRUE),
    spacing    = meta$ild_spacing,
    n_gaps     = n_gaps,
    pct_gap    = meta$ild_spacing$pct_gap
  )
}
