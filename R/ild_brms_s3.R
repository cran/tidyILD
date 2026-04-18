# S3 methods for ild_brms() / brmsfit objects with attr(fit, "ild_data")

#' @rdname ild_tidy
#' @param intervals Logical. For \code{brmsfit} from [ild_brms()], include
#'   posterior \code{rhat}, \code{ess_bulk}, \code{ess_tail} (default \code{TRUE}).
#' @param object Logical. Passed through for non-brms fits; ignored for \code{brmsfit}.
#' @method ild_tidy brmsfit
#' @export
ild_tidy.brmsfit <- function(x, intervals = TRUE, object = FALSE, ...) {
  if (is.null(attr(x, "ild_data", exact = TRUE))) {
    stop(
      "ild_tidy() for brmsfit requires a model fitted with ild_brms() (ild_data attribute missing).",
      call. = FALSE
    )
  }
  rlang::check_installed("brms")
  sf <- summary(x)$fixed
  if (is.null(sf) || nrow(sf) == 0L) {
    stop("No fixed effects in brms summary.", call. = FALSE)
  }
  term <- rownames(sf)
  n <- length(term)
  estimate <- as.numeric(sf[["Estimate"]])
  std_error <- as.numeric(sf[["Est.Error"]])
  conf_low <- as.numeric(sf[["l-95% CI"]])
  conf_high <- as.numeric(sf[["u-95% CI"]])
  p_val <- rep(NA_real_, n)
  stat <- ifelse(is.finite(std_error) & std_error > 0, estimate / std_error, NA_real_)
  int_type <- rep("quantile", n)
  optional <- list()
  if (isTRUE(intervals)) {
    optional$rhat <- as.numeric(sf[["Rhat"]])
    optional$ess_bulk <- as.numeric(sf[["Bulk_ESS"]])
    optional$ess_tail <- as.numeric(sf[["Tail_ESS"]])
  }
  out <- ild_tidy_assemble(
    term = term,
    estimate = estimate,
    std_error = std_error,
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = p_val,
    statistic = stat,
    interval_type = int_type,
    engine = "brms",
    model_class = "brmsfit",
    component = rep("fixed", n),
    effect_level = NULL,
    optional = optional
  )
  if (isTRUE(object)) {
    return(structure(list(meta = list(engine = "brms"), table = out), class = "tidyild_model"))
  }
  out
}

#' @rdname ild_augment
#' @param summary Logical. For \code{brmsfit}, use posterior predictive summary
#'   for fitted values (default \code{TRUE}).
#' @method ild_augment brmsfit
#' @export
ild_augment.brmsfit <- function(x, summary = TRUE, ...) {
  data <- attr(x, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop(
      "ild_augment() for brmsfit requires ild_brms() (ild_data attribute missing).",
      call. = FALSE
    )
  }
  rlang::check_installed("brms")
  validate_ild(data)
  mf <- tryCatch(stats::model.frame(x), error = function(e) stats::model.frame(stats::formula(x), data = data))
  y <- stats::model.response(mf)
  n <- nrow(data)
  if (isTRUE(summary)) {
    fi <- stats::fitted(x, summary = TRUE, ...)
    est <- fi[, "Estimate", drop = TRUE]
    lo <- fi[, "Q2.5", drop = TRUE]
    hi <- fi[, "Q97.5", drop = TRUE]
  } else {
    fi <- stats::fitted(x, summary = FALSE, ...)
    est <- apply(fi, 2L, mean)
    lo <- apply(fi, 2L, stats::quantile, probs = 0.025)
    hi <- apply(fi, 2L, stats::quantile, probs = 0.975)
  }
  y_num <- as.numeric(y)
  if (length(y_num) != n) y_num <- rep(NA_real_, n)
  est <- as.numeric(est)
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  resid <- y_num - est
  r_std <- ild_augment_pearson_residuals(x, n)
  optional <- list(
    .fitted_lower = lo,
    .fitted_upper = hi
  )
  ild_augment_assemble(
    .ild_id = data[[".ild_id"]],
    .ild_time = data[[".ild_time"]],
    .outcome = y_num,
    .fitted = est,
    .resid = resid,
    .resid_std = r_std,
    engine = "brms",
    model_class = ild_tidy_model_class_string(x),
    optional = optional
  )
}

#' @noRd
ild_brms_ppc_summary <- function(fit, ndraws = 500L) {
  dat <- attr(fit, "ild_data", exact = TRUE)
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) mf <- stats::model.frame(stats::formula(fit), data = dat)
  y <- stats::model.response(mf)
  y <- as.numeric(y)
  ndraws <- max(50L, min(as.integer(ndraws), 2000L))
  yrep <- tryCatch(
    brms::posterior_predict(fit, ndraws = ndraws),
    error = function(e) NULL
  )
  if (is.null(yrep)) {
    return(list(error = "posterior_predict failed"))
  }
  mean_obs <- mean(y, na.rm = TRUE)
  sd_obs <- stats::sd(y, na.rm = TRUE)
  mean_rep <- mean(rowMeans(yrep))
  sd_rep <- mean(apply(yrep, 1L, function(z) stats::sd(as.numeric(z), na.rm = TRUE)))
  list(
    n_draws = ndraws,
    mean_obs = mean_obs,
    sd_obs = sd_obs,
    mean_rep = mean_rep,
    sd_rep = sd_rep,
    mean_abs_diff = abs(mean_obs - mean_rep),
    sd_abs_diff = abs(sd_obs - sd_rep)
  )
}

#' @noRd
ild_brms_diagnose_text <- function(conv, samp, ppc) {
  parts <- character()
  if (!is.null(conv) && nrow(conv) > 0L) {
    mx <- suppressWarnings(max(conv$rhat, na.rm = TRUE))
    mn <- suppressWarnings(min(c(conv$ess_bulk, conv$ess_tail), na.rm = TRUE))
    parts <- c(parts, sprintf(
      "Convergence: max R-hat = %.3f; min ESS (bulk/tail) = %.0f.",
      mx, mn
    ))
  }
  if (!is.null(samp)) {
    parts <- c(parts, sprintf(
      "Sampler: %s chains, iter = %s, warmup = %s, adapt_delta = %s, divergent transitions = %s.",
      samp$chains, samp$iter, samp$warmup,
      if (is.na(samp$adapt_delta)) "NA" else samp$adapt_delta,
      if (is.na(samp$n_divergent)) "NA" else samp$n_divergent
    ))
  }
  if (!is.null(ppc) && is.null(ppc$error)) {
    parts <- c(parts, sprintf(
      "PPC (posterior predictive): |mean_obs - mean_rep| = %.4f, |sd_obs - sd_rep| = %.4f.",
      ppc$mean_abs_diff, ppc$sd_abs_diff
    ))
  }
  paste(parts, collapse = " ")
}

#' @export
print.ild_brms_diagnostics <- function(x, ...) {
  cat("ILD Bayesian diagnostics (legacy brms object; use ild_diagnose() for ild_diagnostics_bundle)\n")
  cat(" ", x$summary_text, "\n", sep = "")
  invisible(x)
}

#' @rdname ild_autoplot
#' @method ild_autoplot brmsfit
#' @export
ild_autoplot.brmsfit <- function(x, type = "pp_check", ...) {
  if (is.null(attr(x, "ild_data", exact = TRUE))) {
    stop("ild_autoplot() for brmsfit requires ild_brms().", call. = FALSE)
  }
  rlang::check_installed("brms")
  if (type == "pp_check") {
    return(brms::pp_check(x, ...))
  }
  if (type %in% c("fitted_vs_actual", "fitted")) {
    return(ild_plot(x, type = "fitted_vs_actual", ...))
  }
  stop("Unknown type for ild_autoplot.brmsfit: use 'pp_check' or 'fitted_vs_actual'.", call. = FALSE)
}
