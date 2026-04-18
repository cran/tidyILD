#' Summarize missingness pattern in ILD
#'
#' Returns a tabular summary of missingness by person and/or by variable,
#' plus an optional heatmap plot. Complements [ild_summary()] and supports
#' checking data before modeling. When \code{vars = NULL}, all non-internal
#' data columns are used (observation presence across variables).
#'
#' Also a **section provider** for [ild_diagnose()] (see [ild_diagnostics_utilities]).
#'
#' @section Bundle integration:
#' [ild_diagnose()] passes summaries into \code{data$missing_pattern} (global) and
#' \code{missingness} (model variables) on the \code{\link{ild_diagnostics_bundle}}.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param vars Optional character vector of variable names to summarize.
#'   If \code{NULL}, all non-.ild_* data columns are used.
#' @param max_ids Optional integer. If set, subset to this many persons (sampled)
#'   before computing \code{by_id}, \code{summary}, and \code{plot} to handle large N.
#' @param seed Optional integer. Seed for sampling when \code{max_ids} is set.
#' @param outcome Optional character; if set, \code{by_id} is left-joined with
#'   [ild_missing_compliance()] for that column (interpretable adherence metrics).
#' @param expected_occasions Passed to [ild_missing_compliance()] when \code{outcome} is set.
#' @return A list with: \code{summary} (tibble: one row per var, columns var, n_obs, n_na, pct_na),
#'   \code{plot} (ggplot2 object for missingness heatmap), \code{by_id}, \code{overall},
#'   \code{n_complete}, \code{vars}. When \code{outcome} is set, \code{by_id} includes
#'   compliance columns (see [ild_missing_compliance()]).
#' @seealso [ild_diagnose()], [ild_diagnostics_bundle()]
#' @importFrom dplyr across all_of group_by left_join summarise
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
ild_missing_pattern <- function(x, vars = NULL, max_ids = NULL, seed = NULL,
                                outcome = NULL, expected_occasions = NULL) {
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
  x_work <- x
  if (!is.null(max_ids) && is.finite(max_ids) && max_ids > 0) {
    if (!is.null(seed)) set.seed(seed)
    ids <- unique(x[[".ild_id"]])
    if (length(ids) > max_ids) {
      ids_use <- sample(ids, size = min(max_ids, length(ids)))
      x_work <- x[x[[".ild_id"]] %in% ids_use, ]
    }
  }
  id_col <- ".ild_id"
  time_var <- ".ild_seq"
  by_id <- dplyr::group_by(x_work, .data[[id_col]])
  by_id <- dplyr::summarise(by_id, dplyr::across(
    dplyr::all_of(vars),
    list(n_obs = ~ sum(!is.na(.x)), n_na = ~ sum(is.na(.x))),
    .names = "{.col}_{.fn}"
  ), .groups = "drop")
  overall <- list()
  summary_rows <- list()
  for (v in vars) {
    n_na <- sum(is.na(x_work[[v]]))
    n_obs <- nrow(x_work)
    overall[[v]] <- list(n_na = n_na, n_obs = n_obs, pct_na = 100 * n_na / n_obs)
    summary_rows[[v]] <- tibble::tibble(var = v, n_obs = n_obs, n_na = n_na, pct_na = 100 * n_na / n_obs)
  }
  summary_tbl <- do.call(rbind, summary_rows)
  if (length(summary_rows) > 0) summary_tbl <- tibble::as_tibble(summary_tbl)
  n_complete <- sum(Reduce(`&`, lapply(vars, function(v) !is.na(x_work[[v]]))))
  parts <- lapply(vars, function(v) {
    data.frame(
      id = x_work[[id_col]],
      time = x_work[[time_var]],
      variable = v,
      missing = is.na(x_work[[v]]),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, parts)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = factor(.data$id), fill = .data$missing)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "gray90", "TRUE" = "darkred"),
      labels = c("observed", "missing")
    ) +
    ggplot2::labs(x = "Time (sequence)", y = "Person", fill = NULL, title = "Missingness") +
    ggplot2::theme_minimal()
  if (length(vars) > 1) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$variable), ncol = 1)
  if (!is.null(outcome)) {
    if (!outcome %in% names(x_work)) {
      stop("outcome '", outcome, "' not found in data.", call. = FALSE)
    }
    comp <- ild_missing_compliance(x_work, outcome, expected_occasions = expected_occasions)
    by_id <- dplyr::left_join(by_id, comp, by = ".ild_id")
  }
  list(
    summary = summary_tbl,
    plot = p,
    by_id = by_id,
    overall = overall,
    n_complete = n_complete,
    vars = vars
  )
}
