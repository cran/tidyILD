#' Sequential MSM inverse probability of treatment weights (IPTW for A_t)
#'
#' For **time-varying binary treatment** \eqn{A_t}, fits a separate treatment
#' propensity model at each occasion \eqn{t} (pooling persons at that time):
#' \eqn{P(A_t \mid \bar{L}_t)} where \eqn{\bar{L}_t} is supplied by the user
#' (e.g. lags from [ild_lag()], prior treatment, time-varying confounders). The
#' stabilized factor at each person-occasion is multiplied **within person**
#' over time to yield a **cumulative** MSM IPTW in \code{.ipw_treat}.
#'
#' This differs from [ild_iptw_weights()], which fits **one pooled** logistic
#' over all rows (appropriate for a single treatment decision or descriptive
#' pooling, not the standard sequential MSM estimand for \eqn{\bar{A}_K}).
#'
#' **Assumptions:** Sequential exchangeability given \eqn{\bar{L}_t}, positivity
#' at each \eqn{t}, and correct models. Variance (e.g. bootstrap by person) is
#' not computed here.
#'
#' @section Stabilization:
#' \describe{
#'   \item{\code{marginal}}{Numerator at each occasion uses the sample marginal
#'     \eqn{P(A_t = 1)} among rows at that time (same pattern as pooled stabilized
#'     IPTW).}
#'   \item{\code{prior_treatment}}{Numerator uses \eqn{P(A_t \mid A_{t-1})} from
#'     a logistic model \code{A_t ~ prior_treatment} fit at each occasion (pass
#'     \code{prior_treatment} column name). Denominator remains \eqn{P(A_t \mid \bar{L}_t)}.}
#'   \item{\code{none}}{Unstabilized \eqn{1/P(A_t \mid \bar{L}_t)} for the observed \eqn{A_t}.}
#' }
#'
#' @section Bundle integration:
#' Sets \code{.ipw_treat} for use with [ild_ipcw_weights()] and
#' [ild_joint_msm_weights()].
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param treatment Character. Binary \eqn{A_t} column (\code{0}/\code{1} or two-level factor).
#' @param history Predictors \eqn{\bar{L}_t} for the **denominator** model: a
#'   \code{character} vector of column names, or a one-sided \code{formula}
#'   like \code{~ stress + trt_lag1}. Empty history fits an intercept-only model.
#' @param time_var Character. Occasion index column (default \code{".ild_seq"}).
#' @param stabilize Character: \code{"marginal"}, \code{"prior_treatment"}, or \code{"none"}.
#' @param prior_treatment Character. Required when \code{stabilize = "prior_treatment"}:
#'   name of the \eqn{A_{t-1}} (or prior treatment) column for the numerator model.
#' @param trim Numeric of length 2. Quantiles applied to the **final** cumulative
#'   \code{.ipw_treat} (default \code{c(0.01, 0.99)}). Use length-0 numeric to skip.
#' @param ... Passed to \code{stats::glm()} for each occasion (denominator model;
#'   numerator model when \code{prior_treatment} is used also uses \code{...}).
#' @return \code{x} with \code{.ipw_treat} set to the cumulative sequential weight.
#'   Attributes: \code{ild_iptw_msm_fits} (named list of denominator \code{glm} fits
#'   by occasion), \code{ild_iptw_msm_numerator_fits} (same for numerator when
#'   \code{prior_treatment}).
#' @seealso [ild_iptw_weights()], [ild_ipcw_weights()], [ild_joint_msm_weights()]
#' @export
#' @examples
#' set.seed(11)
#' d <- ild_simulate(n_id = 20, n_obs_per = 6, seed = 11)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.4))
#' d <- ild_prepare(d, id = "id", time = "time")
#' d <- ild_lag(d, stress, mode = "gap_aware", max_gap = Inf)
#' d <- ild_lag(d, trt, mode = "gap_aware", max_gap = Inf)
#' x <- ild_iptw_msm_weights(d, treatment = "trt", history = ~ stress_lag1 + trt_lag1)
#' summary(x$.ipw_treat)
ild_iptw_msm_weights <- function(x,
                                 treatment,
                                 history,
                                 time_var = ".ild_seq",
                                 stabilize = c("marginal", "prior_treatment", "none"),
                                 prior_treatment = NULL,
                                 trim = c(0.01, 0.99),
                                 ...) {
  validate_ild(x)
  stabilize <- match.arg(stabilize)
  if (stabilize == "prior_treatment" && (is.null(prior_treatment) || !nzchar(prior_treatment))) {
    stop("prior_treatment must name a column when stabilize = \"prior_treatment\".", call. = FALSE)
  }
  nms <- names(x)
  if (!treatment %in% nms) stop("treatment '", treatment, "' not found in data.", call. = FALSE)
  if (!time_var %in% nms) stop("time_var '", time_var, "' not found in data.", call. = FALSE)
  if (!is.null(prior_treatment) && !prior_treatment %in% nms) {
    stop("prior_treatment '", prior_treatment, "' not found in data.", call. = FALSE)
  }

  pred_names <- if (inherits(history, "formula")) {
    tryCatch(
      attr(stats::terms(history), "term.labels"),
      error = function(e) character(0)
    )
  } else {
    history
  }
  for (p in pred_names) {
    if (!p %in% nms) stop("predictor '", p, "' not found in data.", call. = FALSE)
  }

  id <- x[[".ild_id"]]
  seqv <- x[[time_var]]
  Araw <- x[[treatment]]
  if (is.factor(Araw)) {
    if (nlevels(Araw) != 2L) {
      stop("Treatment must be binary (two-level factor).", call. = FALSE)
    }
    A <- as.integer(Araw) - 1L
  } else {
    A <- as.numeric(Araw)
    u <- unique(stats::na.omit(A))
    if (length(u) > 2L || !all(u %in% c(0, 1))) {
      stop("Treatment must be binary (0/1 or two-level factor).", call. = FALSE)
    }
  }

  n <- nrow(x)
  r <- rep(NA_real_, n)
  fits_den <- list()
  fits_num <- list()

  rhs_den <- if (length(pred_names) > 0L) paste(pred_names, collapse = " + ") else "1"
  form_den <- stats::as.formula(paste(treatment, "~", rhs_den))

  occ <- sort(unique(as.numeric(seqv)))
  for (t in occ) {
    idx <- which(as.numeric(seqv) == t & !is.na(A))
    if (length(idx) < 2L) next # need variation to fit; rows left NA then neutralized in cumprod
    df <- as.data.frame(x[idx, c(treatment, pred_names), drop = FALSE])
    df[[treatment]] <- A[idx]
    fit_den <- tryCatch(
      stats::glm(form_den, data = df, family = stats::binomial(), ...),
      error = function(e) NULL
    )
    if (is.null(fit_den)) next
    fits_den[[as.character(t)]] <- fit_den
    p_hat <- stats::predict(fit_den, newdata = as.data.frame(x[idx, , drop = FALSE]), type = "response")
    p_hat <- pmax(pmin(p_hat, 1 - 1e-6), 1e-6)
    Ai <- A[idx]

    if (stabilize == "none") {
      ri <- ifelse(Ai >= 0.5, 1 / p_hat, 1 / (1 - p_hat))
    } else if (stabilize == "marginal") {
      p1 <- mean(Ai, na.rm = TRUE)
      p1 <- pmax(pmin(p1, 1 - 1e-6), 1e-6)
      ri <- ifelse(Ai >= 0.5, p1 / p_hat, (1 - p1) / (1 - p_hat))
    } else {
      form_num <- stats::as.formula(paste(treatment, "~", prior_treatment))
      df_num <- as.data.frame(x[idx, c(treatment, prior_treatment), drop = FALSE])
      df_num[[treatment]] <- A[idx]
      fit_num <- tryCatch(
        stats::glm(form_num, data = df_num, family = stats::binomial(), ...),
        error = function(e) NULL
      )
      if (is.null(fit_num)) next
      fits_num[[as.character(t)]] <- fit_num
      num_p <- stats::predict(fit_num, newdata = as.data.frame(x[idx, , drop = FALSE]), type = "response")
      num_p <- pmax(pmin(num_p, 1 - 1e-6), 1e-6)
      ri <- ifelse(Ai >= 0.5, num_p / p_hat, (1 - num_p) / (1 - p_hat))
    }
    r[idx] <- ri
  }

  if (length(fits_den) == 0L) {
    stop(
      "No occasion-level treatment models were fitted (need >=2 rows with non-NA treatment per occasion, or glm failed).",
      call. = FALSE
    )
  }

  ord <- order(id, seqv, seq_len(n))
  ido <- id[ord]
  r_ord <- r[ord]
  w_ord <- rep(NA_real_, n)
  i <- 1L
  while (i <= n) {
    g <- ido[i]
    j <- i
    while (j < n && ido[j + 1L] == g) j <- j + 1L
    idx <- i:j
    rr <- r_ord[idx]
    rr[is.na(rr)] <- 1
    w_ord[idx] <- cumprod(rr)
    i <- j + 1L
  }
  w <- rep(NA_real_, n)
  w[ord] <- w_ord
  if (length(trim) >= 2L && is.numeric(trim)) {
    w <- .ild_ipw_trim(w, trim)
  }
  attrs <- attributes(x)
  x$.ipw_treat <- w
  x <- .ild_ipw_restore_attrs(x, attrs)
  attr(x, "ild_iptw_msm_fits") <- fits_den
  attr(x, "ild_iptw_msm_numerator_fits") <- if (length(fits_num) > 0L) fits_num else NULL
  hist_arg <- if (inherits(history, "formula")) {
    paste(deparse(history, width.cutoff = 500L), collapse = " ")
  } else {
    history
  }
  ild_add_step(x, "ild_iptw_msm_weights",
    list(
      treatment = treatment, history = hist_arg, time_var = time_var,
      stabilize = stabilize, prior_treatment = prior_treatment, trim = trim
    ),
    list(created = ".ipw_treat", mode = "sequential_msm")
  )
}
