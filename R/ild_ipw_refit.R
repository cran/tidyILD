#' Refit an ILD model with inverse-probability weights (sensitivity analysis)
#'
#' Takes a fit from [ild_lme()] (or a formula and data) and refits the model
#' using observation weights from [ild_ipw_weights()]. Only the lme4 (lmer)
#' path is supported; nlme (ar1 = TRUE) is not supported for weighted refits.
#' This is a sensitivity tool, not a full MNAR solution.
#'
#' @param fit_or_formula Either a fitted model from [ild_lme()] (lmerMod), or a
#'   formula. If a formula, \code{data} and \code{weights} are required.
#' @param data An ILD object (see [is_ild()]) containing a weight column (default \code{.ipw})
#'   from [ild_ipw_weights()] or joint MSM weights from [ild_joint_msm_weights()], when refitting
#'   from a fit (or the data to fit when \code{fit_or_formula} is a formula).
#' @param weights Character. Name of the weight column in \code{data} (default \code{".ipw"}).
#' @param ar1 Logical. Must be \code{FALSE} for IPW refit (nlme weighted refit not supported).
#' @param ... Passed to [ild_lme()] or \code{lme4::lmer()}.
#' @return A fitted model (lmerMod) with \code{attr(..., "ild_data")} set so that
#'   [tidy_ild_model()] and [ild_diagnostics()] work. For inference under estimated
#'   weights, see [ild_msm_bootstrap()] and [ild_msm_inference].
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 12, n_obs_per = 10, seed = 1)
#' d$stress <- rnorm(nrow(d))
#' d$mood <- d$y
#' d$mood[sample(nrow(d), 25)] <- NA
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_center(x, mood)
#' mm <- ild_missing_model(x, "mood", "stress")
#' xw <- ild_ipw_weights(x, mm, stabilize = TRUE)
#' fit0 <- ild_lme(mood ~ mood_bp + mood_wp + stress + (1 | id), data = xw,
#'   ar1 = FALSE, warn_no_ar1 = FALSE)
#' fitw <- ild_ipw_refit(fit0, data = xw)
#' tidy_ild_model(fitw)
ild_ipw_refit <- function(fit_or_formula, data, weights = ".ipw", ar1 = FALSE, ...) {
  if (ar1) {
    stop("ild_ipw_refit() does not support ar1 = TRUE (nlme weighted refit not supported). Use ar1 = FALSE.", call. = FALSE)
  }
  validate_ild(data)
  if (!weights %in% names(data)) {
    stop("weights column '", weights, "' not found in data. Run ild_ipw_weights() first.", call. = FALSE)
  }
  wvec <- data[[weights]]
  if (inherits(fit_or_formula, "lmerMod")) {
    formula <- stats::formula(fit_or_formula)
    args <- list(formula = formula, data = data, weights = wvec)
    args <- c(args, list(...))
    fit <- do.call(lme4::lmer, args)
    attr(fit, "ild_data") <- data
    attr(fit, "ild_ar1") <- FALSE
    attr(fit, "ild_provenance") <- ild_new_analysis_provenance(
      fit_or_formula,
      "ild_ipw_refit",
      list(weights = weights),
      list()
    )
    return(fit)
  }
  if (inherits(fit_or_formula, "lme")) {
    stop("ild_ipw_refit() does not support nlme (lme) fits. Refit with ild_lme(..., ar1 = FALSE) then ild_ipw_refit.", call. = FALSE)
  }
  if (inherits(fit_or_formula, "formula")) {
    args <- c(list(formula = fit_or_formula, data = data, weights = wvec), list(...))
    fit <- do.call(lme4::lmer, args)
    attr(fit, "ild_data") <- data
    attr(fit, "ild_ar1") <- FALSE
    attr(fit, "ild_provenance") <- ild_new_analysis_provenance(
      data,
      "ild_ipw_refit",
      list(weights = weights),
      list()
    )
    return(fit)
  }
  stop("fit_or_formula must be an ild_lme() fit (lmerMod) or a formula.", call. = FALSE)
}
