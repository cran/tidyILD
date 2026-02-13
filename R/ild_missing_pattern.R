#' Summarize missingness pattern in ILD
#'
#' Returns a tabular summary of missingness by person and/or by variable.
#' Complements [ild_summary()] and supports checking data before modeling.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param vars Optional character vector of variable names to summarize.
#'   If missing, all non-.ild_* columns (except id/time) are used.
#' @return A list with:
#'   - `by_id`: data frame with one row per person, columns id and for each
#'     var the count (or proportion) of non-missing and missing.
#'   - `overall`: named vector or list of overall missing counts/proportions
#'     per variable.
#'   - `n_complete`: number of rows with no missing in selected vars.
#' @importFrom dplyr across all_of group_by summarise
#' @export
ild_missing_pattern <- function(x, vars = NULL) {
  validate_ild(x)
  ild_cols <- c(".ild_id", ".ild_time", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap")
  all_nms <- names(x)
  data_cols <- setdiff(all_nms, ild_cols)
  if (length(data_cols) == 0) stop("No data columns to summarize for missingness.", call. = FALSE)
  if (!is.null(vars)) {
    vars <- intersect(vars, all_nms)
    if (length(vars) == 0) stop("No selected variables found in data.", call. = FALSE)
  } else {
    vars <- data_cols
  }
  id_col <- ".ild_id"
  by_id <- dplyr::group_by(x, .data[[id_col]])
  by_id <- dplyr::summarise(by_id, dplyr::across(
    dplyr::all_of(vars),
    list(n_obs = ~ sum(!is.na(.x)), n_na = ~ sum(is.na(.x))),
    .names = "{.col}_{.fn}"
  ), .groups = "drop")
  overall <- list()
  for (v in vars) {
    n_na <- sum(is.na(x[[v]]))
    n_obs <- nrow(x)
    overall[[v]] <- list(n_na = n_na, n_obs = n_obs, pct_na = 100 * n_na / n_obs)
  }
  n_complete <- sum(Reduce(`&`, lapply(vars, function(v) !is.na(x[[v]]))))
  list(
    by_id = by_id,
    overall = overall,
    n_complete = n_complete,
    vars = vars
  )
}
