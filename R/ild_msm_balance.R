# MSM / IPW covariate balance, effective sample size, and propensity overlap plots

#' @title MSM and IPW balance diagnostics
#' @name ild_msm_balance_topic
#' @description
#' After building weights ([ild_iptw_weights()], [ild_iptw_msm_weights()], [ild_joint_msm_weights()]),
#' assess **covariate balance** (weighted SMD), **effective sample size** (ESS), and **overlap**
#' of fitted propensities. These complement weight summaries in [fill_diagnostics_causal()] and
#' guardrails on weight ranges.
#'
#' **Weights:** Use \code{.ipw_treat} to check balance for the **treatment** mechanism;
#' use \code{.ipw} for balance in the **joint** MSM pseudo-population used in the weighted outcome
#' model—they need not coincide.
#'
#' @seealso [ild_msm_balance()], [ild_ipw_ess()], [ild_msm_overlap_plot()],
#'   [ild_iptw_weights()], [ild_iptw_msm_weights()], [ild_joint_msm_weights()],
#'   [ild_msm_estimand()], [ild_msm_fit()], [ild_msm_recovery()]
#' @keywords internal
NULL

#' Effective sample size from weights
#'
#' For nonnegative weights \eqn{w}, \eqn{\mathrm{ESS} = (\sum w)^2 / \sum w^2}.
#'
#' @param data An ILD object.
#' @param weights_col Name of the nonnegative weight column (e.g. \code{".ipw"} or \code{".ipw_treat"}).
#' @param by_occasion If \code{TRUE}, one ESS per level of \code{time_var}.
#' @param time_var Column for occasion index (default \code{".ild_seq"}).
#' @return A single numeric (pooled) or a \code{tibble} with \code{occasion} and \code{ess} when
#'   \code{by_occasion = TRUE}.
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 10, n_obs_per = 5, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' x$.ipw <- runif(nrow(x), 0.5, 1.5)
#' ild_ipw_ess(x, ".ipw")
#' ild_ipw_ess(x, ".ipw", by_occasion = TRUE)
ild_ipw_ess <- function(data, weights_col = ".ipw", by_occasion = FALSE,
                        time_var = ".ild_seq") {
  validate_ild(data)
  if (!weights_col %in% names(data)) {
    stop("weights_col '", weights_col, "' not found in data.", call. = FALSE)
  }
  w <- as.numeric(data[[weights_col]])
  w[!is.finite(w) | w < 0] <- NA_real_
  if (!by_occasion) {
    return(.ild_ess_from_w(w))
  }
  if (!time_var %in% names(data)) {
    stop("time_var '", time_var, "' not found in data.", call. = FALSE)
  }
  tv <- data[[time_var]]
  u <- sort(unique(as.numeric(tv)))
  rows <- vector("list", length(u))
  for (i in seq_along(u)) {
    t <- u[i]
    idx <- as.numeric(tv) == t
    rows[[i]] <- tibble::tibble(
      occasion = t,
      ess = .ild_ess_from_w(w[idx])
    )
  }
  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
.ild_ess_from_w <- function(w) {
  w <- w[is.finite(w) & w >= 0]
  if (length(w) == 0L) return(NA_real_)
  sw <- sum(w)
  sw2 <- sum(w^2)
  if (sw2 <= 0) return(NA_real_)
  (sw^2) / sw2
}

#' Standardized mean difference (binary treatment) with IPW-style weights
#'
#' Pooled SD: \eqn{\sqrt{(v_1 + v_0)/2}} where \eqn{v_g} is the weighted variance of \eqn{X}
#' in the weighted treated (\eqn{A=1}) and control (\eqn{A=0}) pseudo-samples.
#'
#' @keywords internal
#' @noRd
.ild_smd_weighted <- function(x, A, w) {
  A <- as.numeric(A)
  if (is.factor(A)) A <- as.integer(A) - 1L
  ok <- is.finite(x) & is.finite(A) & is.finite(w) & w > 0 & A %in% c(0, 1)
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  A <- A[ok]
  w <- w[ok]
  wt1 <- w * A
  wt0 <- w * (1 - A)
  sw1 <- sum(wt1)
  sw0 <- sum(wt0)
  if (sw1 <= 0 || sw0 <= 0) return(NA_real_)
  m1 <- sum(wt1 * x) / sw1
  m0 <- sum(wt0 * x) / sw0
  v1 <- sum(wt1 * (x - m1)^2) / sw1
  v0 <- sum(wt0 * (x - m0)^2) / sw0
  sp <- sqrt((v1 + v0) / 2)
  if (!is.finite(sp) || sp <= 0) return(NA_real_)
  (m1 - m0) / sp
}

#' Covariate balance (weighted SMD) for MSM / IPW
#'
#' Computes **standardized mean differences** between weighted treated and control groups for
#' each named covariate. When \code{by_occasion = TRUE}, balance is computed **within** each
#' occasion (stratum); when \code{FALSE}, all rows are pooled into one pseudo-population.
#'
#' @param data An ILD object with \code{treatment}, \code{covariates}, and \code{weights_col}.
#' @param treatment Character. Binary treatment column (\code{0}/\code{1} or two-level factor).
#' @param covariates Character vector of numeric or binary covariate columns.
#' @param weights_col Analysis weights (e.g. \code{".ipw_treat"} or \code{".ipw"}).
#' @param by_occasion If \code{TRUE}, stratify by \code{time_var}.
#' @param time_var Occasion index column (default \code{".ild_seq"}).
#' @return A \code{tibble} with columns \code{stratum}, \code{covariate}, \code{smd},
#'   \code{mean_treated}, \code{mean_control}, \code{ess_stratum}, \code{weight_col}.
#' @export
#' @examples
#' set.seed(2)
#' d <- ild_simulate(n_id = 12, n_obs_per = 6, seed = 2)
#' d$stress <- rnorm(nrow(d))
#' d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
#' x <- ild_prepare(d, id = "id", time = "time")
#' x$.ipw_treat <- runif(nrow(x), 0.8, 1.2)
#' ild_msm_balance(x, treatment = "trt", covariates = "stress", weights_col = ".ipw_treat")
ild_msm_balance <- function(data,
                            treatment,
                            covariates,
                            weights_col = ".ipw_treat",
                            by_occasion = FALSE,
                            time_var = ".ild_seq") {
  validate_ild(data)
  if (!treatment %in% names(data)) stop("treatment column not found.", call. = FALSE)
  if (!weights_col %in% names(data)) stop("weights_col not found.", call. = FALSE)
  for (cv in covariates) {
    if (!cv %in% names(data)) stop("covariate '", cv, "' not found.", call. = FALSE)
  }
  Araw <- data[[treatment]]
  if (is.factor(Araw)) {
    if (nlevels(Araw) != 2L) stop("treatment must be binary.", call. = FALSE)
    A <- as.integer(Araw) - 1L
  } else {
    A <- as.numeric(Araw)
  }
  w <- as.numeric(data[[weights_col]])
  if (!by_occasion) {
    out <- vector("list", length(covariates))
    ess <- .ild_ess_from_w(w)
    for (i in seq_along(covariates)) {
      cv <- covariates[i]
      x <- as.numeric(data[[cv]])
      smd <- .ild_smd_weighted(x, A, w)
      wt1 <- w * A
      wt0 <- w * (1 - A)
      sw1 <- sum(wt1[is.finite(wt1)], na.rm = TRUE)
      sw0 <- sum(wt0[is.finite(wt0)], na.rm = TRUE)
      m1 <- if (sw1 > 0) sum(wt1 * x, na.rm = TRUE) / sw1 else NA_real_
      m0 <- if (sw0 > 0) sum(wt0 * x, na.rm = TRUE) / sw0 else NA_real_
      out[[i]] <- tibble::tibble(
        stratum = "pooled",
        covariate = cv,
        smd = smd,
        mean_treated = m1,
        mean_control = m0,
        ess_stratum = ess,
        weight_col = weights_col
      )
    }
    return(dplyr::bind_rows(out))
  }
  if (!time_var %in% names(data)) stop("time_var not found.", call. = FALSE)
  tv <- data[[time_var]]
  u <- sort(unique(as.numeric(tv)))
  blocks <- vector("list", 0L)
  for (t in u) {
    idx <- which(as.numeric(tv) == t)
    if (length(idx) < 2L) next
    At <- A[idx]
    wt <- w[idx]
    ess_t <- .ild_ess_from_w(wt)
    for (cv in covariates) {
      x <- as.numeric(data[[cv]][idx])
      smd <- .ild_smd_weighted(x, At, wt)
      wt1 <- wt * At
      wt0 <- wt * (1 - At)
      sw1 <- sum(wt1[is.finite(wt1)], na.rm = TRUE)
      sw0 <- sum(wt0[is.finite(wt0)], na.rm = TRUE)
      m1 <- if (sw1 > 0) sum(wt1 * x, na.rm = TRUE) / sw1 else NA_real_
      m0 <- if (sw0 > 0) sum(wt0 * x, na.rm = TRUE) / sw0 else NA_real_
      blocks[[length(blocks) + 1L]] <- tibble::tibble(
        stratum = as.character(t),
        covariate = cv,
        smd = smd,
        mean_treated = m1,
        mean_control = m0,
        ess_stratum = ess_t,
        weight_col = weights_col
      )
    }
  }
  if (length(blocks) == 0L) {
    return(tibble::tibble(
      stratum = character(),
      covariate = character(),
      smd = numeric(),
      mean_treated = numeric(),
      mean_control = numeric(),
      ess_stratum = numeric(),
      weight_col = character()
    ))
  }
  dplyr::bind_rows(blocks)
}

#' Propensity overlap plot (pooled or sequential MSM IPTW)
#'
#' For **pooled** [ild_iptw_weights()], uses \code{attr(data, "ild_iptw_fit")} and plots the
#' distribution of fitted \eqn{P(A=1\mid L)} by treatment level.
#'
#' For **sequential** [ild_iptw_msm_weights()], uses \code{attr(data, "ild_iptw_msm_fits")} and
#' facets by occasion: fitted denominator propensity at each \eqn{t}.
#'
#' @param data ILD with treatment column and relevant \code{glm} attributes.
#' @param treatment Binary treatment column name.
#' @param source Character: \code{"auto"} (prefer MSM fits if present), \code{"pooled"}, or
#'   \code{"msm"}.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(3)
#'   d <- ild_simulate(n_id = 15, n_obs_per = 5, seed = 3)
#'   d$stress <- rnorm(nrow(d))
#'   d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
#'   x <- ild_prepare(d, id = "id", time = "time")
#'   x <- ild_iptw_weights(x, treatment = "trt", predictors = "stress")
#'   ild_msm_overlap_plot(x, treatment = "trt", source = "pooled")
#' }
ild_msm_overlap_plot <- function(data, treatment, source = c("auto", "pooled", "msm")) {
  validate_ild(data)
  source <- match.arg(source)
  if (!treatment %in% names(data)) stop("treatment not found.", call. = FALSE)
  Araw <- data[[treatment]]
  if (is.factor(Araw)) {
    if (nlevels(Araw) != 2L) stop("treatment must be binary.", call. = FALSE)
    A <- as.integer(Araw) - 1L
  } else {
    A <- as.numeric(Araw)
  }
  has_msm <- !is.null(attr(data, "ild_iptw_msm_fits", exact = TRUE))
  has_pool <- !is.null(attr(data, "ild_iptw_fit", exact = TRUE))
  if (source == "auto") {
    source <- if (has_msm) "msm" else "pooled"
  }
  if (source == "msm") {
    if (!has_msm) {
      stop("Sequential IPTW fits not found. Run ild_iptw_msm_weights() or use source = \"pooled\".",
           call. = FALSE)
    }
    return(.ild_overlap_plot_msm(data, treatment, A))
  }
  if (!has_pool) {
    stop("Pooled IPTW fit not found. Run ild_iptw_weights() or use source = \"msm\" with MSM weights.",
         call. = FALSE)
  }
  fit <- attr(data, "ild_iptw_fit", exact = TRUE)
  p_hat <- stats::predict(fit, newdata = as.data.frame(data), type = "response")
  df <- tibble::tibble(
    p_hat = p_hat,
    treatment = factor(A, levels = c(0, 1), labels = c("A=0", "A=1"))
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$p_hat, color = .data$treatment)) +
    ggplot2::geom_density() +
    ggplot2::labs(
      title = "Propensity overlap (pooled IPTW)",
      x = "Fitted P(A = 1 | L)",
      y = "Density"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
.ild_overlap_plot_msm <- function(data, treatment, A) {
  fits <- attr(data, "ild_iptw_msm_fits", exact = TRUE)
  seqv <- data[[".ild_seq"]]
  if (is.null(seqv)) stop("data must have .ild_seq for MSM overlap plot.", call. = FALSE)
  rows <- list()
  for (nm in names(fits)) {
    t <- as.numeric(nm)
    fit <- fits[[nm]]
    idx <- which(as.numeric(seqv) == t & !is.na(A))
    if (length(idx) < 2L) next
    p_hat <- stats::predict(fit, newdata = as.data.frame(data[idx, , drop = FALSE]), type = "response")
    rows[[length(rows) + 1L]] <- tibble::tibble(
      occasion = t,
      p_hat = p_hat,
      treatment = factor(A[idx], levels = c(0, 1), labels = c("A=0", "A=1"))
    )
  }
  if (length(rows) == 0L) {
    stop("No rows to plot for MSM overlap.", call. = FALSE)
  }
  df <- dplyr::bind_rows(rows)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$p_hat, color = .data$treatment)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ occasion, scales = "free_y") +
    ggplot2::labs(
      title = "Propensity overlap (sequential MSM IPTW)",
      x = "Fitted P(A_t = 1 | history)",
      y = "Density"
    ) +
    ggplot2::theme_minimal()
}
