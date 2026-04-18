# Section-first autoplot for ild_diagnostics_bundle (see ?ild_autoplot)

#' @rdname ild_autoplot
#' @method ild_autoplot ild_diagnostics_bundle
#' @export
ild_autoplot.ild_diagnostics_bundle <- function(x, section = "residual", type = NULL, ...) {
  if (!inherits(x, "ild_diagnostics_bundle")) {
    stop("Expected an ild_diagnostics_bundle.", call. = FALSE)
  }
  section <- match.arg(section, c("residual", "fit", "predictive", "data", "design", "causal"))
  eng <- x$meta$engine
  if (is.null(eng) && !is.null(x$fit)) {
    eng <- x$fit$engine
  }
  is_kfas <- identical(eng, "KFAS")
  is_ctsem <- identical(eng, "ctsem")

  if (identical(section, "residual")) {
    leg <- x$residual$legacy_ild_diagnostics
    if (is.null(type)) {
      if (!is_kfas && !is_ctsem && !is.null(leg)) {
        return(plot_ild_diagnostics(leg, ...))
      }
      type <- "acf"
    }
    type <- match.arg(type, c("acf", "qq", "fitted"))
    if (is_kfas) {
      switch(type,
        acf = plot_bundle_kfas_residual_acf(x),
        qq = plot_bundle_kfas_residual_qq(x),
        fitted = plot_bundle_kfas_residual_fitted(x)
      )
    } else if (is_ctsem) {
      switch(type,
        acf = plot_bundle_ctsem_residual_acf(x),
        qq = plot_bundle_ctsem_residual_qq(x),
        fitted = plot_bundle_ctsem_residual_fitted(x)
      )
    } else {
      switch(type,
        acf = plot_bundle_residual_acf(x),
        qq = plot_bundle_residual_qq(x),
        fitted = plot_bundle_residual_fitted(x)
      )
    }
  } else {
    choices <- switch(section,
      fit = c("convergence", "heterogeneity"),
      predictive = if (is_kfas) c("forecast", "errors") else if (is_ctsem) c("errors") else c("ppc"),
      data = c("missingness"),
      design = c("coverage"),
      causal = c("weights", "overlap")
    )
    if (is.null(type)) {
      type <- choices[1L]
    }
    type <- match.arg(type, choices)
    switch(section,
      fit = if (identical(type, "heterogeneity")) {
        plot_bundle_fit_heterogeneity(x, ...)
      } else if (is_kfas) {
        plot_bundle_kfas_fit_convergence(x)
      } else {
        plot_bundle_fit_convergence(x)
      },
      predictive = if (is_kfas) {
        switch(type,
          forecast = plot_bundle_kfas_predictive_forecast(x),
          errors = plot_bundle_kfas_predictive_errors(x)
        )
      } else if (is_ctsem) {
        plot_bundle_ctsem_predictive_errors(x)
      } else {
        plot_bundle_predictive_ppc(x, ...)
      },
      data = plot_bundle_data_missingness(x),
      design = plot_bundle_design_coverage(x),
      causal = switch(type,
        weights = plot_bundle_causal_weights(x),
        overlap = plot_bundle_causal_overlap(x, ...)
      )
    )
  }
}

#' @keywords internal
#' @noRd
plot_bundle_residual_acf <- function(x) {
  leg <- x$residual$legacy_ild_diagnostics
  if (!is.null(leg)) {
    pl <- plot_ild_diagnostics(leg, type = "residual_acf")
    return(pl$residual_acf)
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Residual ACF requires residual$legacy_ild_diagnostics or a bundle from ",
      "ild_diagnose() (with ild_fit / ild_data attributes). Re-run ild_diagnose().",
      call. = FALSE
    )
  }
  diag <- ild_diagnostics(fit, data = dat, type = "residual_acf", by_id = TRUE)
  plot_ild_diagnostics(diag, type = "residual_acf")$residual_acf
}

#' @keywords internal
#' @noRd
plot_bundle_residual_qq <- function(x) {
  leg <- x$residual$legacy_ild_diagnostics
  if (!is.null(leg)) {
    pl <- plot_ild_diagnostics(leg, type = "qq")
    return(pl$qq)
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Residual Q-Q requires residual$legacy_ild_diagnostics or ild_diagnose() ",
      "with ild_fit / ild_data attributes.",
      call. = FALSE
    )
  }
  diag <- ild_diagnostics(fit, data = dat, type = "qq", by_id = TRUE)
  plot_ild_diagnostics(diag, type = "qq")$qq
}

#' @keywords internal
#' @noRd
plot_bundle_residual_fitted <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(fit) || is.null(dat)) {
    stop(
      "Fitted vs observed requires attr(bundle, \"ild_fit\") and attr(bundle, \"ild_data\"). ",
      "Re-run ild_diagnose() on the model.",
      call. = FALSE
    )
  }
  ild_plot_fitted(fit, dat)
}

#' @keywords internal
#' @noRd
plot_bundle_fit_convergence <- function(x) {
  fd <- x$fit
  eng <- if (!is.null(fd$engine)) fd$engine else x$meta$engine

  if (identical(eng, "brms")) {
    ct <- fd$convergence$convergence_table %||% fd$convergence_table
    if (is.null(ct) || nrow(ct) == 0L) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "Convergence (no fixed-effects summary table)")
      )
    }
    ggplot2::ggplot(ct, ggplot2::aes(
      x = stats::reorder(.data$term, .data$rhat),
      y = .data$rhat
    )) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 1.05, linetype = 2, alpha = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "R-hat", title = "Convergence (fixed effects)") +
      ggplot2::theme_minimal()
  }
  if (identical(eng, "ctsem")) {
    conv <- fd$convergence
    txt <- paste(
      c(
        "Engine: ctsem",
        sprintf("Converged: %s", conv$converged),
        sprintf("Convergence code: %s", conv$convergence_code),
        sprintf("State dimension: %s", fd$state_dimension),
        if (is.finite(fd$log_likelihood)) sprintf("LogLik: %.2f", fd$log_likelihood) else "LogLik: NA"
      ),
      collapse = "\n"
    )
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Convergence (ctsem)") +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)
    )
  }

  fd_conv <- fd$convergence
  sing <- if (!is.null(fd_conv)) fd_conv$singular else fd$singular
  conv <- if (!is.null(fd_conv)) fd_conv$converged else fd$converged
  om <- if (!is.null(fd_conv)) fd_conv$optimizer_messages else fd$optimizer_messages
  txt <- paste(
    c(
      sprintf("Engine: %s", if (!is.null(fd$engine)) fd$engine else eng),
      sprintf("Singular: %s", sing),
      sprintf("Converged: %s", conv),
      if (length(om) > 0L) {
        paste("Optimizer:", paste(om, collapse = "; "))
      } else {
        "Optimizer: (none)"
      }
    ),
    collapse = "\n"
  )
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, hjust = 0.5, vjust = 0.5) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Convergence (frequentist)") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}

#' @keywords internal
#' @noRd
plot_bundle_predictive_ppc <- function(x, ...) {
  eng <- x$meta$engine
  if (!identical(eng, "brms")) {
    stop(
      "Posterior predictive checks (section = \"predictive\", type = \"ppc\") ",
      "require a brmsfit from ild_brms().",
      call. = FALSE
    )
  }
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (is.null(fit)) {
    stop("PPC requires attr(bundle, \"ild_fit\"). Re-run ild_diagnose() on the brmsfit.", call. = FALSE)
  }
  rlang::check_installed("brms")
  brms::pp_check(fit, ...)
}

#' @keywords internal
#' @noRd
plot_bundle_data_missingness <- function(x) {
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop(
      "Missingness plot requires attr(bundle, \"ild_data\"). Re-run ild_diagnose().",
      call. = FALSE
    )
  }
  mp <- ild_missing_pattern(dat, vars = NULL)
  mp$plot
}

#' @keywords internal
#' @noRd
plot_bundle_design_coverage <- function(x) {
  dc <- x$design$ild_design_check
  dat <- attr(x, "ild_data", exact = TRUE)

  if (!is.null(dc$wp_bp) && nrow(dc$wp_bp) > 0L) {
    wp <- dc$wp_bp
    tot <- wp$wp_var + wp$bp_var
    pct_wp <- ifelse(tot > 0, 100 * wp$wp_var / tot, NA_real_)
    pct_bp <- ifelse(tot > 0, 100 * wp$bp_var / tot, NA_real_)
    n <- nrow(wp)
    df <- data.frame(
      variable = rep(as.character(wp$variable), 2L),
      component = rep(c("within-person", "between-person"), each = n),
      pct = c(pct_wp, pct_bp),
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$pct, fill = .data$component)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::labs(
          x = NULL,
          y = "Percent of total variance",
          title = "Design coverage (within vs between)",
          fill = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    )
  }

  if (!is.null(dat) && ".ild_dt" %in% names(dat)) {
    dt <- dat[[".ild_dt"]]
    dt <- dt[!is.na(dt)]
    if (length(dt) == 0L) {
      stop("No non-missing .ild_dt values for interval coverage plot.", call. = FALSE)
    }
    df <- data.frame(dt = dt)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$dt)) +
        ggplot2::geom_histogram(bins = min(30L, max(10L, length(unique(dt)))), fill = "steelblue", color = "white") +
        ggplot2::labs(
          x = expression(Delta * "t (observation intervals)"),
          y = "Count",
          title = "Observation interval coverage",
          subtitle = "WP/BP decomposition unavailable (no model vars in design check)"
        ) +
        ggplot2::theme_minimal()
    )
  }

  stop(
    "Design coverage needs design$ild_design_check$wp_bp or ILD data with `.ild_dt`.",
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
plot_bundle_causal_weights <- function(x) {
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop("Weights plot requires attr(bundle, \"ild_data\"). Re-run ild_diagnose().", call. = FALSE)
  }
  parts <- list()
  if (".ipw" %in% names(dat)) parts[["joint (.ipw)"]] <- dat[[".ipw"]]
  if (".ipw_treat" %in% names(dat)) parts[["IPTW (.ipw_treat)"]] <- dat[[".ipw_treat"]]
  if (".ipw_censor" %in% names(dat)) parts[["IPCW (.ipw_censor)"]] <- dat[[".ipw_censor"]]
  if (length(parts) == 0L) {
    stop(
      "No IPW columns in ild_data. Use ild_ipw_weights(), ild_iptw_weights() + ild_ipcw_weights() + ild_joint_msm_weights(), or add weight columns first.",
      call. = FALSE
    )
  }
  rows <- list()
  for (nm in names(parts)) {
    w <- parts[[nm]]
    w <- w[is.finite(w)]
    if (length(w) > 0L) {
      rows[[length(rows) + 1L]] <- data.frame(source = nm, w = w)
    }
  }
  if (length(rows) == 0L) {
    stop("No finite weight values to plot.", call. = FALSE)
  }
  df <- do.call(rbind, rows)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$w)) +
    ggplot2::geom_histogram(bins = min(30L, max(10L, length(unique(df$w)))), fill = "gray40", color = "white") +
    ggplot2::facet_wrap(~ source, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Weight", y = "Count", title = "Causal / IPW weights") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_causal_overlap <- function(x, treatment, source = "auto", ...) {
  if (missing(treatment) || is.null(treatment) || !nzchar(as.character(treatment)[1L])) {
    stop(
      "Overlap plot requires treatment = <column name> (e.g. treatment = \"trt\").",
      call. = FALSE
    )
  }
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop("Overlap plot requires attr(bundle, \"ild_data\"). Re-run ild_diagnose().", call. = FALSE)
  }
  ild_msm_overlap_plot(dat, treatment = treatment, source = source)
}

#' @keywords internal
#' @noRd
plot_bundle_ctsem_residual_acf <- function(x) {
  acf_tbl <- x$residual$acf
  if (is.null(acf_tbl) || nrow(acf_tbl) == 0L) {
    stop("ctsem residual ACF diagnostics are unavailable in this bundle.", call. = FALSE)
  }
  ggplot2::ggplot(acf_tbl, ggplot2::aes(x = .data$lag, y = .data$acf)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(title = "ctsem residual ACF", x = "Lag", y = "ACF") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_ctsem_residual_qq <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (is.null(fit)) {
    stop("ctsem residual QQ plot requires attr(bundle, \"ild_fit\").", call. = FALSE)
  }
  ag <- ild_augment(fit)
  ggplot2::ggplot(ag, ggplot2::aes(sample = .data$.resid_std)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(title = "ctsem standardized residual QQ plot", x = "Theoretical", y = "Sample") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_ctsem_residual_fitted <- function(x) {
  fit <- attr(x, "ild_fit", exact = TRUE)
  if (is.null(fit)) {
    stop("ctsem fitted diagnostics require attr(bundle, \"ild_fit\").", call. = FALSE)
  }
  ag <- ild_augment(fit)
  ggplot2::ggplot(ag, ggplot2::aes(x = .data$.fitted, y = .data$.resid)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(title = "ctsem residuals vs fitted", x = "Fitted", y = "Residual") +
    ggplot2::theme_minimal()
}

#' @keywords internal
#' @noRd
plot_bundle_ctsem_predictive_errors <- function(x) {
  om <- x$predictive$obs_metrics
  if (is.null(om)) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "ctsem predictive errors unavailable")
    )
  }
  txt <- paste(
    c(
      sprintf("Engine: ctsem"),
      sprintf("n: %s", om$n),
      sprintf("MAE: %.3f", om$mean_abs_error),
      sprintf("RMSE: %.3f", om$rmse),
      sprintf("Mean residual: %.3f", om$mean_residual)
    ),
    collapse = "\n"
  )
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = txt, hjust = 0.5, vjust = 0.5) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "ctsem predictive error summary") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}

#' @keywords internal
#' @noRd
plot_bundle_fit_heterogeneity <- function(x, ...) {
  het <- x$fit$heterogeneity
  if (is.null(het) || !isTRUE(het$available) || is.null(het$object)) {
    stop(
      "Heterogeneity plot requires random effects and a successful ild_heterogeneity() extraction. ",
      "Re-run ild_diagnose() on a model with (e.g.) (predictor | id).",
      call. = FALSE
    )
  }
  dots <- list(...)
  term <- dots$term
  htype <- if (!is.null(dots$heterogeneity_type)) {
    dots$heterogeneity_type
  } else {
    "caterpillar"
  }
  ild_autoplot(het$object, type = htype, term = term)
}
