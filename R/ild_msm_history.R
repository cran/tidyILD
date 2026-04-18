# MSM history-builder utilities (on top of ild_lag)

#' Build a declarative MSM history specification
#'
#' Creates a lightweight spec object used by [ild_build_msm_history()] to generate
#' lagged confounder/treatment history columns with deterministic names (e.g.
#' \code{stress_lag1}, \code{trt_lag2}).
#'
#' @param vars Character vector of variable names to lag.
#' @param lags Integer vector of lag orders. Recycled across \code{vars}. Defaults to \code{1L}.
#' @param mode Passed to [ild_lag()] (default \code{"gap_aware"}).
#' @param max_gap Passed to [ild_lag()] for \code{mode = "gap_aware"}.
#' @return Object of class \code{ild_msm_history_spec}.
#' @export
#' @examples
#' s <- ild_msm_history_spec(vars = c("stress", "trt"), lags = 1:2)
#' s
ild_msm_history_spec <- function(vars,
                                 lags = 1L,
                                 mode = c("gap_aware", "index", "time_window"),
                                 max_gap = Inf) {
  mode <- match.arg(mode)
  if (missing(vars) || length(vars) == 0L) {
    stop("vars must include at least one variable name.", call. = FALSE)
  }
  vars <- unique(as.character(vars))
  lags <- as.integer(lags)
  lags <- unique(lags[is.finite(lags) & lags >= 1L])
  if (length(lags) == 0L) {
    stop("lags must contain at least one positive integer.", call. = FALSE)
  }
  out <- list(
    vars = vars,
    lags = sort(lags),
    mode = mode,
    max_gap = max_gap
  )
  class(out) <- c("ild_msm_history_spec", "list")
  out
}

#' @export
print.ild_msm_history_spec <- function(x, ...) {
  cat("MSM history spec\n")
  cat("  vars: ", paste(x$vars, collapse = ", "), "\n", sep = "")
  cat("  lags: ", paste(x$lags, collapse = ", "), "\n", sep = "")
  cat("  mode: ", x$mode, "\n", sep = "")
  if (!is.null(x$max_gap)) cat("  max_gap: ", x$max_gap, "\n", sep = "")
  invisible(x)
}

#' Build MSM lagged history columns from a spec
#'
#' Applies [ild_lag()] repeatedly according to an [ild_msm_history_spec()] and
#' records a manifest under \code{attr(x, "ild_msm_history_manifest")}.
#'
#' @param data An ILD object.
#' @param spec Object from [ild_msm_history_spec()].
#' @param mode Optional override for \code{spec$mode}.
#' @param max_gap Optional override for \code{spec$max_gap}.
#' @return ILD object with lagged columns added; attributes include
#'   \code{ild_msm_history_manifest} (tibble with \code{variable}, \code{lag},
#'   \code{column}).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 6, n_obs_per = 5, seed = 1)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- rbinom(nrow(d), 1, 0.4)
#' d <- ild_prepare(d, id = "id", time = "time")
#' hs <- ild_msm_history_spec(c("stress", "trt"), lags = 1:2)
#' d2 <- ild_build_msm_history(d, hs)
#' attr(d2, "ild_msm_history_manifest")
ild_build_msm_history <- function(data, spec, mode = NULL, max_gap = NULL) {
  validate_ild(data)
  if (!inherits(spec, "ild_msm_history_spec")) {
    stop("spec must be from ild_msm_history_spec().", call. = FALSE)
  }
  mode_use <- if (is.null(mode)) spec$mode else match.arg(mode, c("gap_aware", "index", "time_window"))
  gap_use <- if (is.null(max_gap)) spec$max_gap else max_gap

  out <- data
  rows <- vector("list", length(spec$vars) * length(spec$lags))
  k <- 0L
  for (v in spec$vars) {
    if (!v %in% names(out)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    for (lg in spec$lags) {
      k <- k + 1L
      # Evaluate ild_lag with a symbol to preserve current naming conventions.
      out <- eval(
        bquote(
          ild_lag(.(out), .(as.name(v)), n = .(lg), mode = .(mode_use), max_gap = .(gap_use))
        )
      )
      rows[[k]] <- tibble::tibble(
        variable = v,
        lag = as.integer(lg),
        column = paste0(v, "_lag", as.integer(lg))
      )
    }
  }
  manifest <- dplyr::bind_rows(rows)
  attr(out, "ild_msm_history_manifest") <- manifest
  out
}
