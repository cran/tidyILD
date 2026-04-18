#' Compute inverse-probability-of-observation weights from a missingness model
#'
#' Uses the fitted missingness model from [ild_missing_model()] to compute
#' weights: unstabilized \code{w = 1 / p_obs} or stabilized
#' \code{w = mean(p_obs) / p_obs}. Weights are trimmed to avoid extremes.
#' Use with [ild_ipw_refit()] for sensitivity analysis. This is diagnostic
#' and sensitivity tooling, not a full MNAR solution.
#'
#' Also a **section provider** for [ild_diagnose()] when \code{.ipw} is present on the
#' analysis data (see [ild_diagnostics_utilities]).
#'
#' @section Bundle integration:
#' Adds \code{.ipw} to \code{x}. When that column is on the ILD data attached to the fit,
#' [ild_diagnose()] fills \code{causal} (weight summaries) on \code{\link{ild_diagnostics_bundle}}.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]); rows must align to the data used to fit \code{miss_fit}.
#' @param miss_fit The return value of [ild_missing_model()] (list with \code{fit}, \code{p_missing}, etc.).
#' @param stabilize Logical. If \code{TRUE} (default), use stabilized weights
#'   \code{mean(p_obs) / p_obs}; otherwise \code{1 / p_obs}.
#' @param trim Numeric of length 2. Quantiles to trim weights (default \code{c(0.01, 0.99)}).
#'   Weights below/above these quantiles are set to the quantile values.
#' @return \code{x} with an added column \code{.ipw} (numeric). ILD attributes are preserved.
#' @seealso [ild_diagnose()], [ild_diagnostics_bundle()], [ild_missing_model()],
#'   [ild_iptw_weights()], [ild_ipcw_weights()], [ild_joint_msm_weights()]
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 15, n_obs_per = 8, seed = 1)
#' d$stress <- rnorm(nrow(d))
#' d$mood <- d$y
#' d$mood[sample(nrow(d), 20)] <- NA
#' x <- ild_prepare(d, id = "id", time = "time")
#' mm <- ild_missing_model(x, "mood", "stress")
#' xw <- ild_ipw_weights(x, mm, stabilize = TRUE)
#' summary(xw$.ipw)
ild_ipw_weights <- function(x, miss_fit, stabilize = TRUE, trim = c(0.01, 0.99)) {
  validate_ild(x)
  if (is.null(miss_fit$fit)) {
    stop("miss_fit has no fit (e.g. no missingness). Use ild_missing_model() first.", call. = FALSE)
  }
  p_missing <- miss_fit$p_missing
  if (is.null(p_missing) || length(p_missing) != nrow(x)) {
    stop("miss_fit p_missing length must match nrow(x). Re-fit ild_missing_model() with this data.", call. = FALSE)
  }
  p_obs <- 1 - p_missing
  p_obs <- pmax(pmin(p_obs, 1), 1e-6)
  w <- if (stabilize) mean(p_obs, na.rm = TRUE) / p_obs else 1 / p_obs
  if (length(trim) >= 2L && is.numeric(trim)) {
    w <- .ild_ipw_trim(w, trim)
  }
  attrs <- attributes(x)
  x$.ipw <- w
  x <- .ild_ipw_restore_attrs(x, attrs)
  x <- ild_add_step(x, "ild_ipw_weights",
    list(stabilize = stabilize, trim = trim),
    list(created = ".ipw")
  )
  x
}
