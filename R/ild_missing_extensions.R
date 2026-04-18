# Person-level compliance, cohort missingness over time, discrete first-event hazard.
# Used by ild_missing_pattern(..., outcome =) and ild_missingness_report().

#' @keywords internal
#' @noRd
.longest_run_true <- function(lgl) {
  if (!length(lgl)) {
    return(0L)
  }
  r <- rle(lgl)
  m <- 0L
  for (i in seq_along(r$values)) {
    if (isTRUE(r$values[i])) {
      m <- max(m, r$lengths[i], na.rm = TRUE)
    }
  }
  as.integer(m)
}

#' @keywords internal
#' @noRd
.monotone_missing_after_first_na <- function(o) {
  na <- is.na(o)
  if (!any(na)) {
    return(NA)
  }
  i <- min(which(na))
  all(na[i:length(na)])
}

#' Person-level missingness compliance for one outcome
#'
#' Per person (after sorting by `.ild_seq`), returns coverage on the outcome,
#' longest streak of observed values, whether missingness is monotone (dropout
#' pattern: once missing, stays missing), and optional comparison to an expected
#' number of occasions.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param outcome Character; column with possible `NA` (e.g. EMA outcome).
#' @param expected_occasions Optional positive integer: planned waves per person
#'   for adherence (`pct_of_expected`). If persons have different row counts,
#'   this is only a rough benchmark; see Details.
#' @return A tibble with columns `.ild_id`, `n_rows`, `n_obs_outcome`,
#'   `pct_nonmissing_outcome`, `longest_run_observed`, `monotone_missing`
#'   (`NA` if no missing values on the outcome for that person), and if
#'   `expected_occasions` is set, `pct_of_expected` and `meets_expected_rows`.
#' @details
#' **Monotone missing** is defined only when the person has at least one missing
#' outcome: all observations after the first missing are missing. Intermittent
#' missing yields `FALSE`. **Discrete hazard** for first missing is provided by
#' [ild_missing_hazard_first()] (cohort-level by `.ild_seq`).
#'
#' @seealso [ild_missing_pattern()], [ild_missingness_report()]
#' @importFrom dplyr arrange bind_rows
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
ild_missing_compliance <- function(x, outcome, expected_occasions = NULL) {
  validate_ild(x)
  if (!outcome %in% names(x)) {
    stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  }
  eo <- NULL
  if (!is.null(expected_occasions)) {
    eo <- as.integer(expected_occasions)[1L]
    if (!is.finite(eo) || eo < 1L) {
      stop("expected_occasions must be a positive integer.", call. = FALSE)
    }
  }
  id_col <- ".ild_id"
  x <- dplyr::arrange(x, .data[[id_col]], .data$.ild_seq)
  sp <- split(as.data.frame(x, stringsAsFactors = FALSE), as.character(x[[id_col]]))
  rows <- lapply(sp, function(d) {
    d <- d[order(d$.ild_seq), , drop = FALSE]
    o <- d[[outcome]]
    n <- length(o)
    n_obs <- sum(!is.na(o))
    lr <- .longest_run_true(!is.na(o))
    mono <- .monotone_missing_after_first_na(o)
    row <- tibble::tibble(
      .ild_id = d[[id_col]][1L],
      n_rows = n,
      n_obs_outcome = as.integer(n_obs),
      pct_nonmissing_outcome = if (n > 0L) 100 * n_obs / n else NA_real_,
      longest_run_observed = lr,
      monotone_missing = mono
    )
    if (!is.null(eo)) {
      row$pct_of_expected <- 100 * n_obs / eo
      row$meets_expected_rows <- n >= eo
    }
    row
  })
  dplyr::bind_rows(rows)
}

#' Cohort-level fraction observed by occasion index
#'
#' For each distinct `.ild_seq` value, computes how many rows exist and what
#' fraction has a non-missing outcome. Optional line plot.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object.
#' @param outcome Character; outcome column.
#' @param plot Logical; if `TRUE`, return a `ggplot2` object in `$plot`.
#' @return A list with `by_occasion` (tibble: `.ild_seq`, `n_rows`, `n_obs`,
#'   `pct_observed`) and if `plot = TRUE`, `plot`.
#' @details
#' Uses **within-study sequence** (`.ild_seq`), not calendar time. For irregular
#' timing, interpret as ordinal wave position rather than equal time spacing.
#'
#' @seealso [ild_missing_pattern()], [ild_missing_compliance()]
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @export
ild_missing_cohort <- function(x, outcome, plot = TRUE) {
  validate_ild(x)
  if (!outcome %in% names(x)) {
    stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  }
  by_occ <- dplyr::group_by(x, .data$.ild_seq)
  by_occ <- dplyr::summarise(
    by_occ,
    n_rows = dplyr::n(),
    n_obs = sum(!is.na(.data[[outcome]])),
    pct_observed = 100 * sum(!is.na(.data[[outcome]])) / dplyr::n(),
    .groups = "drop"
  )
  by_occ <- by_occ[order(by_occ$.ild_seq), , drop = FALSE]
  p <- NULL
  if (isTRUE(plot)) {
    p <- ggplot2::ggplot(by_occ, ggplot2::aes(x = .data$.ild_seq, y = .data$pct_observed)) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::labs(
        x = "Occasion (.ild_seq)",
        y = "% non-missing outcome",
        title = "Cohort observed fraction by occasion"
      ) +
      ggplot2::ylim(0, 100) +
      ggplot2::theme_minimal()
  }
  list(by_occasion = tibble::as_tibble(by_occ), plot = p)
}

#' Discrete hazard of first missing outcome on an ordinal schedule
#'
#' For each `.ild_seq`, estimates the fraction of person-occasions **at risk**
#' (previous occasion observed, or first occasion) that are **missing** on the
#' outcome. This is a coarse discrete-time hazard for **first** missing spell
#' when missingness is intermittent; under **monotone dropout** it matches the
#' usual discrete hazard of dropout.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object.
#' @param outcome Character; outcome column.
#' @return A tibble with `.ild_seq`, `n_at_risk`, `n_missing`, `hazard`.
#' @details
#' **Assumptions:** Rows are ordered within person by `.ild_seq`. The first row
#' per person is always at risk. Later rows are at risk only if the previous
#' row (same person) was non-missing—so a person who is already missing is not
#' counted again. This targets a **first event** narrative; it is not a full
#' repeated-events model.
#'
#' @seealso [ild_missing_cohort()], [ild_missing_compliance()]
#' @importFrom dplyr arrange group_by mutate ungroup summarise
#' @importFrom rlang .data
#' @export
ild_missing_hazard_first <- function(x, outcome) {
  validate_ild(x)
  if (!outcome %in% names(x)) {
    stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  }
  xd <- dplyr::arrange(x, .data$.ild_id, .data$.ild_seq)
  xd <- dplyr::group_by(xd, .data$.ild_id)
  xd <- dplyr::mutate(
    xd,
    prev_obs = dplyr::lag(!is.na(.data[[outcome]]), default = TRUE),
    at_risk = .data$prev_obs,
    event = .data$at_risk & is.na(.data[[outcome]])
  )
  xd <- dplyr::ungroup(xd)
  hz <- dplyr::group_by(xd, .data$.ild_seq)
  hz <- dplyr::summarise(
    hz,
    n_at_risk = sum(.data$at_risk, na.rm = TRUE),
    n_missing = sum(.data$event, na.rm = TRUE),
    hazard = if (sum(.data$at_risk, na.rm = TRUE) > 0) {
      sum(.data$event, na.rm = TRUE) / sum(.data$at_risk, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  )
  hz <- hz[order(hz$.ild_seq), , drop = FALSE]
  tibble::as_tibble(hz)
}
