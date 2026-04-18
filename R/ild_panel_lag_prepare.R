# Multi-variable lag preparation for panel dynamics workflows

#' Prepare several lag columns and audit them
#'
#' Sequentially applies [ild_lag()] to each named variable with the same
#' \code{mode}, \code{max_gap}, \code{window}, and \code{resolution}, then runs
#' [ild_check_lags()] once on all created lag columns. Use this when building
#' multivariate lag specifications (e.g. several predictors or lag orders) with
#' consistent gap or time-window semantics.
#'
#' @param data An ILD object (see [is_ild()]).
#' @param variables Character vector of column names to lag.
#' @param n Integer vector of lag orders, recycled to \code{length(variables)}.
#'   Ignored for \code{mode = "time_window"} (see [ild_lag()]).
#' @param mode,max_gap,window,resolution Passed to each [ild_lag()] call.
#' @return A list with \code{data} (ILD with lag columns), \code{lag_vars}
#'   (names of created columns), \code{check} (tibble from [ild_check_lags()]),
#'   and \code{spec} (list of arguments for provenance).
#' @seealso \code{vignette("temporal-dynamics-model-choice", package = "tidyILD")},
#'   [ild_crosslag()], [ild_lag()].
#' @export
ild_panel_lag_prepare <- function(data,
                                  variables,
                                  n = 1L,
                                  mode = c("gap_aware", "index", "time_window"),
                                  max_gap = NULL,
                                  window = NULL,
                                  resolution = c("closest_prior", "last_in_window", "mean_in_window")) {
  validate_ild(data)
  mode <- match.arg(mode)
  resolution <- match.arg(resolution)
  variables <- unique(as.character(variables))
  if (length(variables) == 0L) {
    stop("variables must be non-empty.", call. = FALSE)
  }
  for (v in variables) {
    if (!v %in% names(data)) {
      stop("Variable '", v, "' not found in data.", call. = FALSE)
    }
  }
  n <- rep(as.integer(n), length.out = length(variables))
  out <- data
  lag_vars <- character(length(variables))
  for (i in seq_along(variables)) {
    v <- variables[i]
    ni <- n[i]
    out <- ild_lag(
      out,
      dplyr::all_of(v),
      n = ni,
      mode = mode,
      max_gap = max_gap,
      window = window,
      resolution = resolution
    )
    lag_vars[i] <- if (mode == "time_window") {
      paste0(v, "_lag_window")
    } else {
      paste0(v, "_lag", ni)
    }
  }
  chk <- ild_check_lags(out, lag_vars = lag_vars, max_gap = max_gap)
  spec <- list(
    variables = variables,
    n = n,
    mode = mode,
    max_gap = max_gap,
    window = window,
    resolution = resolution
  )
  out <- ild_add_step(
    out,
    "ild_panel_lag_prepare",
    spec,
    list(created = lag_vars)
  )
  list(
    data = out,
    lag_vars = lag_vars,
    check = chk,
    spec = spec
  )
}
