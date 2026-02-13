#' Check lag variable validity (gap-aware)
#'
#' Given an ILD object and lag variable names, reports how many lagged
#' values are valid vs invalid (NA because the time distance to the
#' lagged row exceeded a threshold). Useful to audit lag columns before
#' modeling without re-specifying \code{max_gap}.
#'
#' @param x An ILD object (see [is_ild()]) that contains lag columns
#'   (e.g. from [ild_lag()] with \code{mode = "gap_aware"}).
#' @param lag_vars Character vector of lag column names (e.g. \code{"y_lag1"}).
#'   If \code{NULL}, attempts to detect columns ending in \code{_lag{n}}.
#' @param max_gap Numeric. Threshold used to define invalid (same units as
#'   \code{.ild_time_num}). If \code{NULL}, uses \code{ild_meta(x)$ild_gap_threshold}.
#' @return A data frame with one row per lag variable: \code{var}, \code{n_valid},
#'   \code{n_invalid}, \code{n_first} (rows that are first per person, so no lag),
#'   \code{n_total}, \code{pct_valid} (among rows that could have a lag, i.e. excluding first).
#' @importFrom tibble tibble as_tibble
#' @export
ild_check_lags <- function(x, lag_vars = NULL, max_gap = NULL) {
  validate_ild(x)
  if (is.null(lag_vars)) {
    nms <- names(x)
    lag_vars <- nms[grepl("_lag[0-9]+$", nms)]
  }
  if (length(lag_vars) == 0) {
    return(tibble::tibble(
      var = character(), n_valid = integer(), n_invalid = integer(),
      n_first = integer(), n_total = integer(), pct_valid = double()
    ))
  }
  if (is.null(max_gap)) {
    meta <- ild_meta(x)
    max_gap <- meta$ild_gap_threshold
    if (is.null(max_gap) || !is.finite(max_gap)) max_gap <- Inf
  }
  id_col <- ".ild_id"
  seq_col <- ".ild_seq"
  dt_col <- ".ild_dt"
  n_first <- sum(x[[seq_col]] == 1L, na.rm = TRUE)
  out <- vector("list", length(lag_vars))
  j <- 0L
  for (i in seq_along(lag_vars)) {
    v <- lag_vars[i]
    if (!v %in% names(x)) {
      warning("Lag variable '", v, "' not found in data.", call. = FALSE)
      next
    }
    j <- j + 1L
    could_have_lag <- x[[seq_col]] > 1L
    n_valid <- sum(!is.na(x[[v]]) & could_have_lag, na.rm = TRUE)
    n_invalid <- sum(is.na(x[[v]]) & could_have_lag, na.rm = TRUE)
    n_total <- sum(could_have_lag, na.rm = TRUE)
    pct_valid <- if (n_total > 0) 100 * n_valid / n_total else NA_real_
    out[[j]] <- list(
      var = v,
      n_valid = as.integer(n_valid),
      n_invalid = as.integer(n_invalid),
      n_first = as.integer(n_first),
      n_total = as.integer(n_total),
      pct_valid = pct_valid
    )
  }
  if (j == 0) {
    return(tibble::tibble(
      var = character(), n_valid = integer(), n_invalid = integer(),
      n_first = integer(), n_total = integer(), pct_valid = double()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(out[seq_len(j)], as.data.frame, stringsAsFactors = FALSE)))
}
