# KFAS visualization: bundle autoplot helpers and user-facing ild_plot_* functions

#' Plot smoothed latent states (first state by default)
#'
#' @param x An object from [ild_kfas()].
#' @param state_index Integer; which column of \code{alphahat} to plot (default \code{1L}).
#' @param ... Passed to [ggplot2::labs()].
#' @return A \code{ggplot} object.
#' @export
ild_plot_states <- function(x, state_index = 1L, ...) {
  rlang::check_installed("KFAS", reason = "to plot ild_kfas states")
  if (!inherits(x, "ild_fit_kfas")) {
    stop("ild_plot_states() expects an ild_fit_kfas object from ild_kfas().", call. = FALSE)
  }
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) stop("Missing attr(fit, \"ild_data\").", call. = FALSE)
  ah <- x$kfs$alphahat
  if (is.null(ah) || !is.matrix(ah) || ncol(ah) < state_index) {
    stop("Smoothed states (alphahat) not available.", call. = FALSE)
  }
  tidx <- seq_len(nrow(dat))
  df <- data.frame(
    time = tidx,
    state = as.numeric(ah[, state_index]),
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$state)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::labs(
      title = sprintf("Smoothed state (%s)", colnames(ah)[state_index] %||% paste0("state_", state_index)),
      x = "Time index",
      y = "Estimate",
      ...
    ) +
    ggplot2::theme_minimal()
}

#' Plot filtered vs smoothed state (first state)
#'
#' Compares one-step filtered state (\code{att}) with smoothed (\code{alphahat}) when available.
#'
#' @param x An object from [ild_kfas()].
#' @return A \code{ggplot} object.
#' @export
ild_plot_filtered_vs_smoothed <- function(x) {
  rlang::check_installed("KFAS", reason = "to plot ild_kfas states")
  if (!inherits(x, "ild_fit_kfas")) {
    stop("ild_plot_filtered_vs_smoothed() expects ild_fit_kfas.", call. = FALSE)
  }
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) stop("Missing attr(fit, \"ild_data\").", call. = FALSE)
  kfs <- x$kfs
  ah <- kfs$alphahat
  att <- kfs$att
  if (is.null(ah) || !is.matrix(ah)) {
    stop("Smoothed states not available.", call. = FALSE)
  }
  sm <- as.numeric(ah[, 1L])
  n <- nrow(dat)
  tidx <- seq_len(n)
  if (!is.null(att) && is.matrix(att) && nrow(att) >= n) {
    ft <- as.numeric(att[seq_len(n), 1L])
  } else {
    ft <- rep(NA_real_, n)
  }
  df <- data.frame(
    time = tidx,
    smoothed = sm,
    filtered = ft,
    stringsAsFactors = FALSE
  )
  df_long <- rbind(
    data.frame(time = df$time, value = df$smoothed, series = "smoothed", stringsAsFactors = FALSE),
    data.frame(time = df$time, value = df$filtered, series = "filtered", stringsAsFactors = FALSE)
  )
  ggplot2::ggplot(df_long, ggplot2::aes(x = .data$time, y = .data$value, colour = .data$series)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(
      title = "Filtered vs smoothed (first latent state)",
      x = "Time index",
      y = "Estimate",
      colour = NULL
    ) +
    ggplot2::theme_minimal()
}

#' Plot forecast or future simulation (stub when no horizon)
#'
#' When \code{forecast_horizon > 0} was used in [ild_kfas()], attempts a short
#' ahead forecast via \pkg{KFAS}; otherwise returns an informative empty panel.
#'
#' @param x An object from [ild_kfas()].
#' @param ... Passed to \code{\link[stats]{predict}} for the fitted \pkg{KFAS} model when forecasting is available.
#' @return A \code{ggplot} object.
#' @export
ild_plot_forecast <- function(x, ...) {
  rlang::check_installed("KFAS", reason = "to forecast ild_kfas")
  if (!inherits(x, "ild_fit_kfas")) {
    stop("ild_plot_forecast() expects ild_fit_kfas.", call. = FALSE)
  }
  fh <- x$mapping$forecast_horizon
  if (is.null(fh) || fh < 1L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5, y = 0.5,
          label = "No forecast requested (forecast_horizon = 0).\nPass forecast_horizon > 0 to ild_kfas() when implemented.",
          hjust = 0.5, vjust = 0.5
        ) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Forecast") +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)
    )
  }
  pred <- tryCatch(
    stats::predict(x$kfas_model, n.ahead = as.integer(fh), ...),
    error = function(e) NULL
  )
  if (is.null(pred)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Forecast unavailable", hjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Forecast") +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)
    )
  }
  p <- as.numeric(pred)
  df <- data.frame(h = seq_along(p), y = p, stringsAsFactors = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$h, y = .data$y)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Horizon", y = "Forecast", title = "KFAS forecast (mean)") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_residual_acf <- function(x) {
  rlang::check_installed("KFAS")
  ac <- x$residual$acf %||% x$residual$acf_innovations
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (!is.null(ac) && is.list(ac) && !is.null(ac$lag) && !is.null(ac$acf)) {
    df <- data.frame(lag = ac$lag, acf = ac$acf, stringsAsFactors = FALSE)
  } else if (inherits(fit, "ild_fit_kfas")) {
    innov <- tryCatch(as.numeric(stats::rstandard(fit$kfs)), error = function(e) NULL)
    if (is.null(innov) || length(innov) < 2L) {
      v <- fit$kfs$v
      Fv <- fit$kfs$F
      if (!is.null(v) && !is.null(Fv)) innov <- as.numeric(v) / sqrt(pmax(as.numeric(Fv), 1e-12))
    }
    if (is.null(innov) || length(innov) < 2L) {
      stop("Could not extract innovations for ACF.", call. = FALSE)
    }
    a <- stats::acf(innov, plot = FALSE, lag.max = min(20L, length(innov) - 1L))
    la <- a$lag
    acf <- a$acf
    if (is.matrix(la)) la <- as.numeric(la[, 1L]) else as.numeric(la)
    if (is.matrix(acf)) acf <- as.numeric(acf[, 1L]) else as.numeric(acf)
    df <- data.frame(lag = la, acf = acf, stringsAsFactors = FALSE)
  } else {
    stop("KFAS bundle residual ACF requires KFAS diagnostics or ild_fit_kfas.", call. = FALSE)
  }
  ggplot2::ggplot(df, ggplot2::aes(x = .data$lag, y = .data$acf)) +
    ggplot2::geom_col(width = 0.02) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "ACF of standardized innovations",
      x = "Lag",
      y = "ACF"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_residual_qq <- function(x) {
  rlang::check_installed("KFAS")
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (!inherits(fit, "ild_fit_kfas")) {
    stop("KFAS Q-Q requires bundle from ild_diagnose(ild_kfas_fit).", call. = FALSE)
  }
  innov <- tryCatch(as.numeric(stats::rstandard(fit$kfs)), error = function(e) NULL)
  if (is.null(innov)) {
    v <- fit$kfs$v
    Fv <- fit$kfs$F
    if (!is.null(v) && !is.null(Fv)) innov <- as.numeric(v) / sqrt(pmax(as.numeric(Fv), 1e-12))
  }
  if (is.null(innov) || length(innov) < 3L) {
    stop("Could not extract standardized innovations for Q-Q.", call. = FALSE)
  }
  qq <- stats::qqnorm(innov, plot = FALSE)
  df <- data.frame(x = qq$x, y = qq$y, stringsAsFactors = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
    ggplot2::labs(
      title = "Normal Q-Q of standardized innovations",
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_residual_fitted <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (!inherits(fit, "ild_fit_kfas") || is.null(dat)) {
    stop("KFAS fitted vs signal requires ild_fit_kfas and ild_data on bundle.", call. = FALSE)
  }
  resp <- attr(fit, "ild_response", exact = TRUE)
  y <- dat[[resp]]
  y <- if (is.numeric(y)) y else suppressWarnings(as.numeric(y))
  ex <- ild_kfas_extract_augment(fit$kfs, y, TRUE)
  tidx <- seq_len(nrow(dat))
  df <- data.frame(
    time = tidx,
    y = y,
    fitted = ex$fitted,
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$y, colour = "Observed")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted, colour = "Signal")) +
    ggplot2::labs(
      title = "Outcome vs signal expectation",
      x = "Time index",
      y = NULL,
      colour = NULL
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_fit_convergence <- function(x) {
  fd <- x$fit
  meta <- x$meta
  ll <- fd$log_likelihood
  if (is.null(ll)) ll <- NA_real_
  opt <- fd$optimizer
  conv <- if (!is.null(fd$convergence)) fd$convergence$converged else NA
  meth <- if (!is.null(opt$method)) opt$method else "?"
  txt <- paste(
    c(
      sprintf("Engine: %s", if (!is.null(fd$engine)) fd$engine else "KFAS"),
      sprintf("State spec: %s", if (!is.null(meta$state_spec)) meta$state_spec else "?"),
      sprintf("Observation family: %s", if (!is.null(meta$observation_family)) meta$observation_family else "gaussian"),
      sprintf("Log-likelihood: %s", if (is.finite(ll)) format(round(ll, 4)) else "NA"),
      sprintf("Optimizer: %s", meth),
      sprintf("Converged: %s", if (isTRUE(conv)) "TRUE" else if (isFALSE(conv)) "FALSE" else "NA"),
      sprintf("Pool mode: %s", if (!is.null(meta$pool_mode)) meta$pool_mode else "none"),
      sprintf("Fit context: %s", if (!is.null(meta$fit_context)) meta$fit_context else "single_series")
    ),
    collapse = "\n"
  )
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 1, label = txt, hjust = 0, vjust = 1, size = 3.2) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "KFAS model fit (MLE)") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1.05)
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_predictive_forecast <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (!inherits(fit, "ild_fit_kfas")) {
    stop("KFAS forecast plot requires ild_fit_kfas.", call. = FALSE)
  }
  ild_plot_forecast(fit)
}

#' @keywords internal
#' @noRd
plot_bundle_kfas_predictive_errors <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (!inherits(fit, "ild_fit_kfas") || is.null(dat)) {
    stop("KFAS errors plot requires bundle from ild_diagnose(ild_kfas_fit).", call. = FALSE)
  }
  rlang::check_installed("KFAS")
  innov <- tryCatch(as.numeric(stats::rstandard(fit$kfs)), error = function(e) NULL)
  if (is.null(innov)) {
    v <- fit$kfs$v
    Fv <- fit$kfs$F
    if (!is.null(v) && !is.null(Fv)) innov <- as.numeric(v) / sqrt(pmax(as.numeric(Fv), 1e-12))
  }
  if (is.null(innov)) {
    stop("Standardized innovations unavailable.", call. = FALSE)
  }
  n <- min(length(innov), nrow(dat))
  df <- data.frame(
    time = seq_len(n),
    abs_innovation = abs(innov[seq_len(n)]),
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$abs_innovation)) +
    ggplot2::geom_line(colour = "coral") +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::labs(
      title = "One-step |standardized innovation|",
      x = "Time index",
      y = "|rstandard|"
    ) +
    ggplot2::theme_minimal()
}
