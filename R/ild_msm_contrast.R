# MSM post-fit contrasts

#' Compute MSM contrasts over time from a fitted weighted model
#'
#' Computes marginal treatment contrasts by occasion using model fixed effects.
#' This helper is intentionally standalone: it accepts either an \code{ild_msm_fit}
#' object or a weighted \code{lmerMod} with attached \code{ild_data}.
#'
#' @param object Output from [ild_msm_fit()] or a \code{lmerMod}.
#' @param treatment Optional treatment column name. Defaults to
#'   \code{object$estimand$treatment} for \code{ild_msm_fit}, otherwise required.
#' @param time_var Occasion column (default \code{".ild_seq"}).
#' @param target_time Optional target subset: \code{"all"} (default), \code{"final"},
#'   or numeric vector of occasions.
#' @param conf_level Confidence level for Wald intervals (default \code{0.95}).
#' @return Tibble with `ild_tidy_schema` columns plus \code{method},
#'   \code{terms}, and \code{target_time}.
#' @export
ild_msm_contrast_over_time <- function(object,
                                       treatment = NULL,
                                       time_var = ".ild_seq",
                                       target_time = "all",
                                       conf_level = 0.95) {
  fit <- object
  data <- NULL
  inf_method <- "model_wald"
  est_target_time <- NULL
  if (inherits(object, "ild_msm_fit")) {
    fit <- object$fit
    data <- object$weights_data
    if (is.null(treatment)) treatment <- object$estimand$treatment
    est_target_time <- object$estimand$target_time
    if (!is.null(object$inference) && !is.null(object$inference$method)) {
      inf_method <- as.character(object$inference$method)[1L]
    }
  }
  if (!inherits(fit, "lmerMod")) stop("object must contain a lmerMod fit.", call. = FALSE)
  if (is.null(data)) data <- attr(fit, "ild_data", exact = TRUE)
  if (is.null(data)) stop("Unable to find ILD data (attr(fit, \"ild_data\")).", call. = FALSE)
  if (is.null(treatment) || !treatment %in% names(data)) {
    stop("treatment column must be supplied and present in data.", call. = FALSE)
  }
  if (!time_var %in% names(data)) {
    stop("time_var not found in data.", call. = FALSE)
  }
  if (identical(target_time, "all") && !is.null(est_target_time)) {
    target_time <- est_target_time
  }
  beta <- lme4::fixef(fit)
  vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(vc)) stop("Could not compute vcov for contrast SEs.", call. = FALSE)
  f_fix <- lme4::nobars(stats::formula(fit))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  tt <- sort(unique(data[[time_var]]))
  if (is.character(target_time)) {
    tgt <- tolower(as.character(target_time)[1L])
    if (identical(tgt, "final")) {
      tt <- tt[length(tt)]
    } else if (!identical(tgt, "all")) {
      stop("target_time must be \"all\", \"final\", or numeric vector.", call. = FALSE)
    }
  } else {
    tnum <- as.numeric(target_time)
    tt <- tt[tt %in% tnum]
  }
  if (!length(tt)) {
    stop("No observed occasions matched target_time.", call. = FALSE)
  }
  rows <- vector("list", length(tt))
  for (i in seq_along(tt)) {
    tval <- tt[[i]]
    d_t <- data[data[[time_var]] == tval, , drop = FALSE]
    d1 <- d_t
    d0 <- d_t
    d1[[treatment]] <- 1
    d0[[treatment]] <- 0
    X1 <- stats::model.matrix(f_fix, data = d1)
    X0 <- stats::model.matrix(f_fix, data = d0)
    cm <- intersect(colnames(X1), names(beta))
    if (!length(cm)) {
      stop("Could not align fixed-effect design columns with coefficient names.", call. = FALSE)
    }
    X1 <- X1[, cm, drop = FALSE]
    X0 <- X0[, cm, drop = FALSE]
    b_use <- beta[cm]
    vc_use <- vc[cm, cm, drop = FALSE]
    dbar <- colMeans(X1 - X0)
    est <- as.numeric(sum(dbar * b_use))
    se <- as.numeric(sqrt(drop(dbar %*% vc_use %*% dbar)))
    stat <- est / se
    pval <- 2 * (1 - stats::pnorm(abs(stat)))
    rows[[i]] <- tibble::tibble(
      term = paste0("contrast_t", tval),
      estimate = est,
      std_error = se,
      conf_low = est - z * se,
      conf_high = est + z * se,
      p_value = pval,
      statistic = stat,
      interval_type = "Wald",
      method = inf_method,
      component = "fixed",
      effect_level = "population",
      engine = "lmer",
      model_class = ild_tidy_model_class_string(fit),
      target_time = as.numeric(tval)
    )
  }
  out <- dplyr::bind_rows(rows)
  for (nm in ILD_TIDY_OPTIONAL_COLS) out[[nm]] <- NA_real_
  out$terms <- out$term
  out
}
