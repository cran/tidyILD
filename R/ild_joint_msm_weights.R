#' Joint MSM weights from IPTW and IPCW components
#'
#' Multiplies \code{.ipw_treat} and \code{.ipw_censor} into a single analysis
#' weight column (default \code{.ipw}) for use with [ild_ipw_refit()] and
#' [ild_diagnose()]. Optional scaling sets the mean joint weight to 1.
#'
#' @section Bundle integration:
#' Sets \code{.ipw} (or \code{joint_name}) so existing causal diagnostics and
#' guardrails apply to the **joint** weight.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object containing \code{.ipw_treat} and \code{.ipw_censor}
#'   (from [ild_iptw_weights()] or [ild_iptw_msm_weights()], and [ild_ipcw_weights()]).
#' @param stabilize Character. \code{"none"}: \code{w_treat * w_censor} only.
#'   \code{"mean1"} (default): divide joint weights by their mean so the mean is 1
#'   (helps comparability with stabilized components).
#' @param trim Numeric of length 2. Quantiles for trimming the **joint** vector
#'   (default \code{c(0.01, 0.99)}).
#' @param joint_name Character. Name of the combined weight column (default \code{".ipw"}).
#' @return \code{x} with \code{joint_name} set; component columns unchanged.
#' @seealso [ild_iptw_weights()], [ild_iptw_msm_weights()], [ild_ipcw_weights()], [ild_ipw_refit()]
#' @export
#' @examples
#' set.seed(4)
#' d <- ild_simulate(n_id = 15, n_obs_per = 8, seed = 4)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- as.integer(d$stress > 0)
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_iptw_weights(x, "trt", "stress")
#' x <- ild_ipcw_weights(x, "stress")
#' x <- ild_joint_msm_weights(x)
#' summary(x$.ipw)
ild_joint_msm_weights <- function(x,
                                  stabilize = c("mean1", "none"),
                                  trim = c(0.01, 0.99),
                                  joint_name = ".ipw") {
  validate_ild(x)
  stabilize <- match.arg(stabilize)
  if (!".ipw_treat" %in% names(x)) {
    stop("Column '.ipw_treat' not found. Run ild_iptw_weights() first.", call. = FALSE)
  }
  if (!".ipw_censor" %in% names(x)) {
    stop("Column '.ipw_censor' not found. Run ild_ipcw_weights() first.", call. = FALSE)
  }
  wt <- x$.ipw_treat
  wc <- x$.ipw_censor
  if (length(wt) != nrow(x) || length(wc) != nrow(x)) {
    stop("Component weight length must match nrow(x).", call. = FALSE)
  }
  w <- wt * wc
  if (stabilize == "mean1") {
    m <- mean(w, na.rm = TRUE)
    if (is.finite(m) && m > 0) w <- w / m
  }
  if (length(trim) >= 2L && is.numeric(trim)) {
    w <- .ild_ipw_trim(w, trim)
  }
  attrs <- attributes(x)
  x[[joint_name]] <- w
  x <- .ild_ipw_restore_attrs(x, attrs)
  ild_add_step(x, "ild_joint_msm_weights",
    list(stabilize = stabilize, trim = trim, joint_name = joint_name),
    list(created = joint_name)
  )
}
