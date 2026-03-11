# Compare two pipelines (data or analysis) by provenance
# Uses ild_provenance(), ild_flatten_provenance() from ild_provenance.R, ild_methods.R

#' Compare provenance of two objects and report differences
#'
#' Flattens provenance for each object (data steps plus analysis steps), then
#' compares step sequences and key arguments. Useful to compare preprocessing
#' pipelines, model settings, or to see what changed between two analyses.
#'
#' @param x1 First object (ILD data, model fit, or diagnostics with provenance).
#' @param x2 Second object (same types as x1).
#' @return A list of class \code{ild_compare_pipelines} with \code{only_in_first}
#'   (step names only in x1), \code{only_in_second}, \code{differing} (list of
#'   per-step arg differences), and \code{summary} (character vector of
#'   human-readable differences). If either object has no provenance, returns
#'   a list with \code{message} and empty comparison components.
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
#' x1 <- ild_prepare(d, id = "id", time = "time")
#' x1 <- ild_center(x1, y)
#' x2 <- ild_prepare(d, id = "id", time = "time")
#' x2 <- ild_center(x2, y)
#' ild_compare_pipelines(x1, x2)
ild_compare_pipelines <- function(x1, x2) {
  p1 <- ild_provenance(x1)
  p2 <- ild_provenance(x2)
  if (is.null(p1) && is.null(p2)) {
    out <- list(
      only_in_first = character(),
      only_in_second = character(),
      differing = list(),
      summary = character(),
      message = "Neither object has provenance."
    )
    class(out) <- "ild_compare_pipelines"
    return(out)
  }
  if (is.null(p1)) {
    out <- list(
      only_in_first = character(),
      only_in_second = character(),
      differing = list(),
      summary = "First object has no provenance.",
      message = "x1 has no recorded provenance."
    )
    class(out) <- "ild_compare_pipelines"
    return(out)
  }
  if (is.null(p2)) {
    out <- list(
      only_in_first = character(),
      only_in_second = character(),
      differing = list(),
      summary = "Second object has no provenance.",
      message = "x2 has no recorded provenance."
    )
    class(out) <- "ild_compare_pipelines"
    return(out)
  }
  steps1 <- ild_flatten_provenance(p1)
  steps2 <- ild_flatten_provenance(p2)
  names1 <- vapply(steps1, function(s) s$step, character(1L))
  names2 <- vapply(steps2, function(s) s$step, character(1L))
  only_first <- setdiff(names1, names2)
  only_second <- setdiff(names2, names1)
  common <- intersect(names1, names2)
  differing <- list()
  summary_lines <- character()
  for (step_name in common) {
    idx1 <- which(names1 == step_name)[1L]
    idx2 <- which(names2 == step_name)[1L]
    if (is.na(idx1) || is.na(idx2)) next
    a1 <- steps1[[idx1]]$args
    a2 <- steps2[[idx2]]$args
    if (is.null(a1)) a1 <- list()
    if (is.null(a2)) a2 <- list()
    all_keys <- unique(c(names(a1), names(a2)))
    for (k in all_keys) {
      v1 <- a1[[k]]
      v2 <- a2[[k]]
      if (identical(v1, v2)) next
      differing <- c(differing, list(list(step = step_name, arg = k, value1 = v1, value2 = v2)))
      v1_str <- if (length(v1) > 1) paste(v1, collapse = ", ") else as.character(v1)
      v2_str <- if (length(v2) > 1) paste(v2, collapse = ", ") else as.character(v2)
      if (nchar(v1_str) > 40) v1_str <- paste0(substr(v1_str, 1, 37), "...")
      if (nchar(v2_str) > 40) v2_str <- paste0(substr(v2_str, 1, 37), "...")
      summary_lines <- c(summary_lines, paste0("Different ", k, " (", step_name, "): ", v1_str, " vs ", v2_str))
    }
  }
  if (length(only_first) > 0) {
    summary_lines <- c(summary_lines, paste0("Only in first: ", paste(only_first, collapse = ", ")))
  }
  if (length(only_second) > 0) {
    summary_lines <- c(summary_lines, paste0("Only in second: ", paste(only_second, collapse = ", ")))
  }
  if (length(summary_lines) == 0 && length(only_first) == 0 && length(only_second) == 0) {
    summary_lines <- "Pipelines match (same steps and arguments)."
  }
  out <- list(
    only_in_first = only_first,
    only_in_second = only_second,
    differing = differing,
    summary = summary_lines
  )
  class(out) <- "ild_compare_pipelines"
  out
}

#' @export
print.ild_compare_pipelines <- function(x, ...) {
  if (!is.null(x$message)) {
    cat(x$message, "\n")
    return(invisible(x))
  }
  cat("Pipeline comparison\n")
  if (length(x$only_in_first) > 0) {
    cat("  Only in first: ", paste(x$only_in_first, collapse = ", "), "\n", sep = "")
  }
  if (length(x$only_in_second) > 0) {
    cat("  Only in second: ", paste(x$only_in_second, collapse = ", "), "\n", sep = "")
  }
  for (s in x$summary) {
    cat("  ", s, "\n", sep = "")
  }
  invisible(x)
}
