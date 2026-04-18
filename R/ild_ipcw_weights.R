#' Inverse probability of censoring weights (IPCW) for monotone dropout
#'
#' Builds **discrete-time** IPCW weights for **monotone loss-to-follow-up**:
#' each person contributes a sequence of observed visits; after the last visit
#' the person is censored. This differs from [ild_missing_model()] + [ild_ipw_weights()],
#' which model **sporadic item missingness** on an outcome.
#'
#' Internally, each person-occasion row is labeled with an indicator
#' \code{drop_next}: whether the person drops before the next scheduled occasion
#' (including the last observed row). A pooled logistic regression models
#' \code{drop_next} given covariates; stabilized or unstabilized inverse
#' probabilities are accumulated within person (product over follow-up) to yield
#' \code{.ipw_censor}.
#'
#' **Assumptions:** Monotone censoring, positivity, correct censoring model.
#'
#' @section Bundle integration:
#' Adds \code{.ipw_censor}. Combine with [ild_iptw_weights()] or
#' [ild_iptw_msm_weights()] via [ild_joint_msm_weights()].
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param predictors Character vector of covariate names (time-varying or baseline
#'   carried forward in \code{x}).
#' @param stabilize Logical. If \code{TRUE} (default), multiply factors
#'   \code{mean(1 - p_drop) / (1 - p_drop)} at each row before cumulating; if
#'   \code{FALSE}, use \code{1 / (1 - p_drop)}.
#' @param trim Numeric of length 2. Quantiles applied to **final** cumulative
#'   \code{.ipw_censor} (default \code{c(0.01, 0.99)}).
#' @param ... Passed to \code{stats::glm()}.
#' @return \code{x} with added column \code{.ipw_censor}. Attribute
#'   \code{ild_ipcw_fit} holds the fitted \code{glm}.
#' @seealso [ild_iptw_weights()], [ild_joint_msm_weights()]
#' @export
#' @examples
#' set.seed(3)
#' d <- ild_simulate(n_id = 18, n_obs_per = 7, seed = 3)
#' d$stress <- rnorm(nrow(d))
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_ipcw_weights(x, predictors = "stress")
#' summary(x$.ipw_censor)
ild_ipcw_weights <- function(x, predictors, stabilize = TRUE,
                             trim = c(0.01, 0.99), ...) {
  validate_ild(x)
  nms <- names(x)
  for (p in predictors) {
    if (!p %in% nms) stop("predictor '", p, "' not found in data.", call. = FALSE)
  }
  id <- x[[".ild_id"]]
  seqv <- x[[".ild_seq"]]
  n <- nrow(x)
  ord <- order(id, seqv)
  ido <- id[ord]
  seqo <- seqv[ord]
  drop_o <- rep(NA_real_, n)
  i <- 1L
  while (i <= n) {
    g <- ido[i]
    j <- i
    while (j < n && ido[j + 1L] == g) j <- j + 1L
    for (k in i:j) {
      if (k == j) {
        drop_o[k] <- 1
      } else {
        sk <- as.numeric(seqo[k])
        sn <- as.numeric(seqo[k + 1L])
        drop_o[k] <- if (sn == sk + 1L) 0 else 1
      }
    }
    i <- j + 1L
  }
  drop_next <- rep(NA_real_, n)
  drop_next[ord] <- drop_o
  id_col <- ".ild_id"
  pred_cols <- if (length(predictors) > 0L) c(id_col, predictors) else id_col
  df <- as.data.frame(x[, pred_cols, drop = FALSE])
  df$.ild_drop_next <- drop_next
  rhs <- if (length(predictors) > 0L) paste(predictors, collapse = " + ") else "1"
  form <- stats::as.formula(paste(".ild_drop_next ~", rhs))
  fit <- stats::glm(form, data = df, family = stats::binomial(), ...)
  p_drop <- stats::predict(fit, newdata = df, type = "response")
  p_drop <- pmax(pmin(p_drop, 1 - 1e-6), 1e-6)
  p_stay <- 1 - p_drop
  if (stabilize) {
    ms <- mean(p_stay, na.rm = TRUE)
    ms <- max(ms, 1e-6)
    contrib <- ms / p_stay
  } else {
    contrib <- 1 / p_stay
  }
  contrib_ord <- contrib[ord]
  w_ord <- rep(NA_real_, n)
  i <- 1L
  while (i <= n) {
    g <- ido[i]
    j <- i
    while (j < n && ido[j + 1L] == g) j <- j + 1L
    idx <- i:j
    w_ord[idx] <- cumprod(contrib_ord[idx])
    i <- j + 1L
  }
  w <- rep(NA_real_, n)
  w[ord] <- w_ord
  if (length(trim) >= 2L && is.numeric(trim)) {
    w <- .ild_ipw_trim(w, trim)
  }
  attrs <- attributes(x)
  x$.ipw_censor <- w
  x <- .ild_ipw_restore_attrs(x, attrs)
  attr(x, "ild_ipcw_fit") <- fit
  ild_add_step(x, "ild_ipcw_weights",
    list(predictors = predictors, stabilize = stabilize, trim = trim),
    list(created = ".ipw_censor")
  )
}
