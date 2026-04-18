# Recovery summaries from ild_power(..., return_sims = TRUE)

#' Recovery metrics from simulation replications
#'
#' Summarizes **bias**, **RMSE**, and **nominal Wald interval coverage** for a known
#' coefficient using the per-replication table from [ild_power()] when
#' \code{return_sims = TRUE}. Purely descriptive; does not run new simulations.
#'
#' @param sim_results A data frame or tibble with columns \code{estimate},
#'   \code{std_error}, and \code{converged} (as in \code{ild_power(..., return_sims = TRUE)$sim_results}).
#' @param truth Known value of the estimand (e.g. \code{effect_size} in [ild_power()]).
#' @param level Nominal coverage level for Wald intervals (default \code{0.95}).
#' @param converged_only If \code{TRUE} (default), only rows with \code{converged == TRUE}
#'   are used.
#' @return A one-row \code{\link[tibble]{tibble}} with \code{truth}, \code{n} (rows used),
#'   \code{n_total} (rows in \code{sim_results} if \code{converged_only}), \code{mean_estimate},
#'   \code{bias}, \code{rmse}, \code{coverage}, \code{level}.
#' @seealso [ild_power()]
#' @export
ild_recovery_metrics <- function(sim_results,
                                 truth,
                                 level = 0.95,
                                 converged_only = TRUE) {
  req <- c("estimate", "std_error", "converged")
  nm <- names(sim_results)
  if (!all(req %in% nm)) {
    stop(
      "sim_results must contain columns: ", paste(req, collapse = ", "),
      call. = FALSE
    )
  }
  truth <- as.numeric(truth)[1L]
  level <- as.numeric(level)[1L]
  if (level <= 0 || level >= 1) {
    stop("level must be in (0, 1).", call. = FALSE)
  }
  n_total <- nrow(sim_results)
  sim <- if (isTRUE(converged_only)) {
    sim_results[sim_results$converged %in% TRUE, , drop = FALSE]
  } else {
    sim_results
  }
  n <- nrow(sim)
  if (n == 0L) {
    return(tibble::tibble(
      truth = truth,
      n = 0L,
      n_total = n_total,
      mean_estimate = NA_real_,
      bias = NA_real_,
      rmse = NA_real_,
      coverage = NA_real_,
      level = level
    ))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  est <- sim$estimate
  se <- sim$std_error
  lo <- est - z * se
  hi <- est + z * se
  covers <- !is.na(se) & se > 0 & truth >= lo & truth <= hi
  tibble::tibble(
    truth = truth,
    n = n,
    n_total = n_total,
    mean_estimate = mean(est, na.rm = TRUE),
    bias = mean(est - truth, na.rm = TRUE),
    rmse = sqrt(mean((est - truth)^2, na.rm = TRUE)),
    coverage = mean(covers, na.rm = TRUE),
    level = level
  )
}
