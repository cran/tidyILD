#' Inverse probability of treatment weights (IPTW)
#'
#' Fits a **single pooled** treatment propensity model (logistic regression)
#' over all person-occasions and computes stabilized or unstabilized IPTW weights
#' (one factor per row). Use this for a **time-invariant** treatment or descriptive
#' pooling. For **time-varying** \eqn{A_t} with a sequential MSM estimand, use
#' [ild_iptw_msm_weights()] instead. Outcome missingness IPW is
#' [ild_missing_model()] + [ild_ipw_weights()].
#'
#' **Assumptions:** Positivity (\eqn{0 < P(A|X) < 1}), correct specification of
#' the treatment model. This is sensitivity / MSM-style tooling, not a substitute
#' for careful causal design.
#'
#' @section Bundle integration:
#' Adds \code{.ipw_treat}. Use [ild_joint_msm_weights()] to combine with
#' \code{.ipw_censor} into \code{.ipw} for [ild_ipw_refit()] and [ild_diagnose()].
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param treatment Character. Name of a **binary** treatment column (numeric
#'   \code{0}/\code{1} or two-level factor).
#' @param predictors Character vector of covariate names in \code{x} (main terms only).
#' @param stabilize Logical. If \code{TRUE} (default), stabilized weights
#'   \eqn{P(A)/P(A|X)} for the observed \eqn{A}; otherwise \eqn{1/P(A|X)}.
#' @param trim Numeric of length 2. Quantiles for trimming (default \code{c(0.01, 0.99)}).
#' @param ... Passed to \code{stats::glm()}.
#' @return \code{x} with added column \code{.ipw_treat}. Attributes include
#'   \code{ild_iptw_fit} (the fitted \code{glm} object).
#' @seealso [ild_iptw_msm_weights()], [ild_ipcw_weights()], [ild_joint_msm_weights()],
#'   [ild_ipw_refit()]
#' @export
#' @examples
#' set.seed(2)
#' d <- ild_simulate(n_id = 20, n_obs_per = 6, seed = 2)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- as.integer(d$stress > 0)
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
#' summary(x$.ipw_treat)
ild_iptw_weights <- function(x, treatment, predictors, stabilize = TRUE,
                             trim = c(0.01, 0.99), ...) {
  validate_ild(x)
  nms <- names(x)
  if (!treatment %in% nms) {
    stop("treatment '", treatment, "' not found in data.", call. = FALSE)
  }
  for (p in predictors) {
    if (!p %in% nms) stop("predictor '", p, "' not found in data.", call. = FALSE)
  }
  Araw <- x[[treatment]]
  if (is.factor(Araw)) {
    if (nlevels(Araw) != 2L) {
      stop("ild_iptw_weights() currently supports binary treatment only (two-level factor).", call. = FALSE)
    }
    A <- as.integer(Araw) - 1L
  } else {
    A <- as.numeric(Araw)
    u <- unique(stats::na.omit(A))
    if (length(u) > 2L || !all(u %in% c(0, 1))) {
      stop("ild_iptw_weights() currently supports binary treatment (0/1 or two-level factor).", call. = FALSE)
    }
  }
  id_col <- ".ild_id"
  pred_cols <- if (length(predictors) > 0L) c(id_col, predictors) else id_col
  df <- as.data.frame(x[, pred_cols, drop = FALSE])
  df[[treatment]] <- A
  rhs <- if (length(predictors) > 0L) paste(predictors, collapse = " + ") else "1"
  form <- stats::as.formula(paste(treatment, "~", rhs))
  fit <- stats::glm(form, data = df, family = stats::binomial(), ...)
  p_hat <- stats::predict(fit, newdata = df, type = "response")
  p_hat <- pmax(pmin(p_hat, 1 - 1e-6), 1e-6)
  p1 <- mean(A, na.rm = TRUE)
  p1 <- pmax(pmin(p1, 1 - 1e-6), 1e-6)
  if (stabilize) {
    w <- ifelse(A >= 0.5, p1 / p_hat, (1 - p1) / (1 - p_hat))
  } else {
    w <- ifelse(A >= 0.5, 1 / p_hat, 1 / (1 - p_hat))
  }
  if (length(trim) >= 2L && is.numeric(trim)) {
    w <- .ild_ipw_trim(w, trim)
  }
  attrs <- attributes(x)
  x$.ipw_treat <- w
  x <- .ild_ipw_restore_attrs(x, attrs)
  attr(x, "ild_iptw_fit") <- fit
  ild_add_step(x, "ild_iptw_weights",
    list(treatment = treatment, predictors = predictors, stabilize = stabilize, trim = trim),
    list(created = ".ipw_treat")
  )
}
