#' ILD design diagnostics: spacing, WP/BP, missingness, and recommendations
#'
#' Aggregates [ild_summary()], [ild_spacing()], [ild_spacing_class()], and
#' optionally [ild_decomposition()] and [ild_missing_pattern()] into one
#' design summary. Use before modeling to see spacing class, correlation
#' recommendation, within- vs between-person variance, and missingness.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param vars Optional character vector of variable names for decomposition
#'   and missingness. If \code{NULL}, only spacing and summary are computed;
#'   \code{wp_bp} and \code{missingness} will be \code{NULL}.
#' @return A list of class \code{ild_design_check}: \code{summary} (from ild_summary),
#'   \code{spacing_class} (regular-ish / irregular-ish), \code{spacing} (from ild_spacing),
#'   \code{recommendation} (AR1/CAR1 text), \code{wp_bp} (decomposition tibble or NULL),
#'   \code{missingness} (list with \code{summary} tibble and \code{pct_na} overall, or NULL).
#'   Use \code{print()} for a human-readable summary.
#' @export
#' @examples
#' d <- ild_simulate(n_id = 10, n_obs_per = 8, irregular = TRUE, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
#' ild_design_check(x, vars = "y")
ild_design_check <- function(x, vars = NULL) {
  validate_ild(x)
  sum_out <- ild_summary(x)
  spacing_out <- ild_spacing(x)
  spacing_class <- ild_spacing_class(x)
  wp_bp <- NULL
  missingness <- NULL
  if (!is.null(vars)) {
    vars <- intersect(vars, names(x))
    ild_cols <- c(".ild_id", ".ild_time", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap")
    vars <- setdiff(vars, ild_cols)
    if (length(vars) > 0) {
      wp_bp <- ild_decomposition(x, dplyr::all_of(vars))
      miss_out <- ild_missing_pattern(x, vars = vars)
      pct_na <- if (nrow(miss_out$summary) > 0) mean(miss_out$summary$pct_na) else NA_real_
      missingness <- list(summary = miss_out$summary, pct_na = pct_na)
    }
  }
  out <- list(
    summary = sum_out$summary,
    spacing_class = spacing_class,
    spacing = spacing_out,
    recommendation = spacing_out$recommendation,
    wp_bp = wp_bp,
    missingness = missingness
  )
  class(out) <- c("ild_design_check", "list")
  out
}

#' @export
print.ild_design_check <- function(x, ...) {
  cat("ILD design check\n")
  cat("---------------\n")
  if (!is.null(x$summary) && nrow(x$summary) > 0) {
    cat("N persons:", x$summary$n_id[1], "| N obs:", x$summary$n_obs[1], "\n")
  }
  cat("Spacing class:", x$spacing_class, "\n")
  cat("Recommendation:", x$recommendation, "\n")
  if (!is.null(x$wp_bp) && nrow(x$wp_bp) > 0) {
    tot_var <- x$wp_bp$wp_var + x$wp_bp$bp_var
    pct_wp <- ifelse(tot_var > 0, 100 * x$wp_bp$wp_var / tot_var, NA_real_)
    pct_bp <- ifelse(tot_var > 0, 100 * x$wp_bp$bp_var / tot_var, NA_real_)
    cat("Within-person variance:", round(pct_wp[1], 1), "%\n")
    cat("Between-person variance:", round(pct_bp[1], 1), "%\n")
  }
  if (!is.null(x$missingness)) {
    cat("Missingness (mean pct across vars):", round(x$missingness$pct_na, 1), "%\n")
  }
  invisible(x)
}
