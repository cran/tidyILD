# Internal fillers for ild_diagnostics_bundle assembly (engine-agnostic contract).
# Standalone section providers are documented under ?ild_diagnostics_utilities
# (ild_design_check, ild_missing_pattern, ild_missing_model, ild_ipw_weights).

#' Flatten nested \code{fit} slice for guardrails and legacy callers
#' @param engine Optional engine label (\code{"lmer"}, \code{"lme"}, \code{"brms"}, \code{"ctsem"}) when
#'   \code{fit$engine} is missing (e.g. unit-test fixtures).
#' @keywords internal
#' @noRd
fit_bundle_flat_for_guardrails <- function(fit, engine = NULL) {
  if (is.null(fit) || !is.list(fit)) {
    return(list(
      singular = NA,
      max_rhat = NA_real_,
      convergence_table = NULL,
      ild_posterior = NULL
    ))
  }
  eng <- engine %||% fit$engine
  if (identical(eng, "brms")) {
    conv <- fit$convergence
    if (inherits(conv, "data.frame")) {
      conv <- list(convergence_table = conv, max_rhat = fit$max_rhat)
    }
    if (is.null(conv) || length(conv) == 0L) {
      conv <- list(
        convergence_table = fit$convergence_table,
        max_rhat = fit$max_rhat
      )
    }
    ps <- fit$posterior_summary$ild_posterior %||% fit$ild_posterior
    list(
      singular = NA,
      max_rhat = conv$max_rhat %||% fit$max_rhat,
      convergence_table = conv$convergence_table %||% fit$convergence_table,
      ild_posterior = ps
    )
  } else if (identical(eng, "ctsem")) {
    conv <- fit$convergence
    list(
      singular = NA,
      max_rhat = NA_real_,
      convergence_table = NULL,
      ild_posterior = NULL,
      converged = tryCatch(conv$converged, error = function(e) NA),
      drift_abs_max = fit$drift_abs_max %||% NA_real_
    )
  } else {
    conv <- fit$convergence
    if (inherits(conv, "data.frame")) {
      conv <- list(singular = fit$singular, converged = fit$converged, optimizer_messages = fit$optimizer_messages)
    }
    if (is.null(conv) || length(conv) == 0L) {
      conv <- list(singular = fit$singular, converged = fit$converged, optimizer_messages = fit$optimizer_messages)
    }
    list(
      singular = conv$singular %||% fit$singular,
      max_rhat = NA_real_,
      convergence_table = NULL,
      ild_posterior = NULL
    )
  }
}

#' Enrich \code{data} / \code{design} with parallel semantic blocks (cohort, per_id, timing, imbalance).
#' @keywords internal
#' @noRd
enrich_bundle_semantic_sections <- function(bundle) {
  if (!is.null(bundle$data)) {
    d <- bundle$data
    if (is.null(d$per_id) && !is.null(d$obs_per_id)) {
      bundle$data$per_id <- d$obs_per_id
    }
    if (is.null(d$imbalance) && !is.null(d$obs_per_id)) {
      bundle$data$imbalance <- d$obs_per_id
    }
    if (is.null(d$timing)) {
      bundle$data$timing <- list(
        spacing_class = d$spacing_class,
        n_gaps = d$n_gaps,
        pct_gap = d$pct_gap,
        median_dt_sec = d$median_dt_sec,
        iqr_dt_sec = d$iqr_dt_sec,
        time_range = d$time_range
      )
    }
    if (is.null(d$distribution)) {
      bundle$data$distribution <- list(
        outcome_summaries = d$outcome_summaries,
        missingness_rates = d$missingness_rates
      )
    }
  }
  if (!is.null(bundle$design)) {
    g <- bundle$design
    if (!is.null(bundle$data$cohort)) {
      bundle$design$cohort <- bundle$data$cohort
    }
    if (!is.null(bundle$data$obs_per_id)) {
      bundle$design$per_id <- bundle$data$obs_per_id
    }
    if (is.null(g$timing) && !is.null(g$time_coverage)) {
      bundle$design$timing <- list(
        time_coverage = g$time_coverage,
        spacing_class = g$spacing_class
      )
    }
    if (is.null(g$imbalance) && !is.null(g$occasion_imbalance)) {
      bundle$design$imbalance <- g$occasion_imbalance
    }
  }
  bundle
}

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Data-layer diagnostics (ILD object)
#' @param outcome_vars Optional character vector of column names for \code{outcome_summaries} and \code{missingness_rates}.
#' @keywords internal
#' @noRd
fill_diagnostics_data <- function(data, include_missing_pattern = TRUE, outcome_vars = NULL) {
  validate_ild(data)
  sm <- ild_summary(data)
  sc <- ild_spacing_class(data)
  mp <- NULL
  if (isTRUE(include_missing_pattern)) {
    mp <- tryCatch(ild_missing_pattern(data, vars = NULL), error = function(e) NULL)
  }
  n_int <- if (!is.null(sm$spacing$n_intervals)) sm$spacing$n_intervals else {
    sum(!is.na(data[[".ild_dt"]]))
  }
  cohort <- list(
    n_id = as.integer(sm$summary$n_id[1]),
    n_obs = as.integer(sm$summary$n_obs[1]),
    n_intervals = as.integer(n_int)
  )
  tab <- as.integer(table(data[[".ild_id"]]))
  obs_per_id <- list(
    min = as.integer(min(tab)),
    median = stats::median(tab),
    max = as.integer(max(tab)),
    sd = if (length(tab) > 1L) stats::sd(as.numeric(tab)) else NA_real_
  )
  outcome_summaries <- NULL
  missingness_rates <- NULL
  if (!is.null(outcome_vars) && length(outcome_vars) > 0L) {
    outcome_vars <- unique(outcome_vars[outcome_vars %in% names(data)])
    if (length(outcome_vars) > 0L) {
      rows <- lapply(outcome_vars, function(v) {
        x <- data[[v]]
        ntot <- length(x)
        nna <- sum(is.na(x))
        tibble::tibble(
          variable = v,
          mean = if (is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_,
          sd = if (is.numeric(x)) stats::sd(x, na.rm = TRUE) else NA_real_,
          min = if (is.numeric(x)) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
          max = if (is.numeric(x)) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_,
          pct_na = if (ntot > 0L) 100 * nna / ntot else NA_real_,
          n = ntot
        )
      })
      outcome_summaries <- do.call(rbind, rows)
      missingness_rates <- tibble::tibble(
        variable = outcome_vars,
        pct_na = vapply(outcome_vars, function(v) {
          x <- data[[v]]
          if (length(x) == 0L) return(NA_real_)
          100 * mean(is.na(x))
        }, double(1L))
      )
    }
  }
  list(
    summary = sm$summary,
    spacing_class = sc,
    n_gaps = sm$n_gaps,
    pct_gap = sm$pct_gap,
    median_dt_sec = sm$summary$median_dt_sec[1],
    iqr_dt_sec = sm$summary$iqr_dt_sec[1],
    time_range = sm$time_range,
    cohort = cohort,
    obs_per_id = obs_per_id,
    outcome_summaries = outcome_summaries,
    missingness_rates = missingness_rates,
    missing_pattern = if (!is.null(mp)) {
      list(summary = mp$summary, overall = mp$overall, n_complete = mp$n_complete)
    } else {
      NULL
    }
  )
}

#' Design-layer diagnostics
#' @keywords internal
#' @noRd
fill_diagnostics_design <- function(data, vars = NULL) {
  validate_ild(data)
  dc <- ild_design_check(data, vars = vars)
  sm <- ild_summary(data)
  tr <- sm$time_range
  span_sec <- if (length(tr) >= 2L && all(is.finite(tr))) (tr[2L] - tr[1L]) else NA_real_
  time_coverage <- list(
    min = if (length(tr) >= 1L) tr[1L] else NA_real_,
    max = if (length(tr) >= 2L) tr[2L] else NA_real_,
    span_sec = span_sec
  )
  has_wp_bp <- !is.null(dc$wp_bp) && (inherits(dc$wp_bp, "data.frame") || inherits(dc$wp_bp, "tbl_df")) &&
    nrow(dc$wp_bp) > 0L
  flags <- list(
    has_wp_bp = has_wp_bp,
    spacing_class = dc$spacing_class,
    irregular = identical(dc$spacing_class, "irregular-ish")
  )
  list(
    ild_design_check = dc,
    spacing_class = dc$spacing_class,
    recommendation = dc$recommendation,
    wp_bp = dc$wp_bp,
    design_missingness = dc$missingness,
    flags = flags,
    time_coverage = time_coverage
  )
}

#' Fit diagnostics -- lmerMod
#' @keywords internal
#' @noRd
fill_diagnostics_fit_lmerMod <- function(fit) {
  rescor_note <- "lme4::lmer has no Gaussian AR1/CAR1 residual correlation; use nlme::lme via ild_lme(ar1 = TRUE)."
  out <- list(
    engine = "lmer",
    convergence = list(
      singular = NA,
      converged = NA,
      optimizer_messages = character()
    ),
    rank = list(fixed_effects_X = NA_integer_),
    optimizer = list(
      reml = NA,
      name = NA_character_,
      optinfo = NULL
    ),
    residual_correlation = list(
      modeled = FALSE,
      structure = "none",
      note = rescor_note
    ),
    random_effects_structure = list(theta = NULL)
  )
  if (!inherits(fit, "lmerMod")) return(out)
  out$convergence$singular <- lme4::isSingular(fit)
  out$optimizer$reml <- tryCatch(lme4::getME(fit, "is_REML"), error = function(e) NA)
  out$optimizer$name <- tryCatch(as.character(fit@optinfo$optimizer)[1L], error = function(e) NA_character_)
  th <- tryCatch(lme4::getME(fit, "theta"), error = function(e) NULL)
  if (!is.null(th) && length(th) > 0L) {
    out$random_effects_structure$theta <- list(
      length = length(th),
      min = min(th, na.rm = TRUE),
      max = max(th, na.rm = TRUE)
    )
  }
  X <- tryCatch(lme4::getME(fit, "X"), error = function(e) NULL)
  if (!is.null(X) && nrow(X) > 0L) {
    out$rank$fixed_effects_X <- tryCatch(qr(X)$rank, error = function(e) NA_integer_)
  }
  ar1 <- isTRUE(attr(fit, "ild_ar1", exact = TRUE))
  out$residual_correlation <- list(
    modeled = ar1,
    structure = if (ar1) "ild_ar1_attr" else "none",
    note = rescor_note
  )
  opt <- fit@optinfo
  out$optimizer$optinfo <- list(
    conv = opt$conv,
    finopt = opt$finopt
  )
  cv <- opt$conv$lme4
  if (!is.null(cv)) {
    if (is.list(cv)) {
      nums <- suppressWarnings(as.numeric(unlist(cv, use.names = FALSE)))
      out$convergence$converged <- length(nums) > 0L && all(nums == 0, na.rm = TRUE)
    } else {
      out$convergence$converged <- isTRUE(as.integer(cv)[1L] == 0L)
    }
  }
  if (length(opt$warnings) > 0L) {
    out$convergence$optimizer_messages <- vapply(
      opt$warnings,
      function(w) tryCatch(conditionMessage(w), error = function(e) "warning"),
      character(1L)
    )
  }
  out$heterogeneity <- fill_diagnostics_heterogeneity(fit)
  out
}

#' Fit diagnostics -- nlme lme
#' @keywords internal
#' @noRd
fill_diagnostics_fit_lme <- function(fit) {
  out <- list(
    engine = "lme",
    convergence = list(
      singular = NA,
      converged = NA,
      apVar_ok = NA,
      optimizer_messages = character()
    ),
    rank = list(fixed_effects_X = NA_integer_),
    optimizer = list(note = "nlme::lme -- see model object for optimization details."),
    residual_correlation = list(modeled = FALSE, class = NA_character_, coef_corStruct = NULL),
    random_effects_structure = list(note = "See VarCorr(model) and random effects in summary().")
  )
  if (!inherits(fit, "lme")) return(out)
  out$convergence$converged <- TRUE
  out$convergence$apVar_ok <- !is.null(fit$apVar) && !any(is.na(fit$apVar))
  if (!is.null(fit$apVar) && any(is.na(fit$apVar))) {
    out$convergence$singular <- TRUE
  } else {
    out$convergence$singular <- FALSE
  }
  corr_class <- attr(fit, "ild_correlation_class", exact = TRUE)
  coef_cs <- tryCatch(
    if (!is.null(fit$modelStruct$corStruct)) coef(fit$modelStruct$corStruct) else NULL,
    error = function(e) NULL
  )
  has_cor <- !is.null(fit$modelStruct$corStruct)
  out$residual_correlation <- list(
    modeled = isTRUE(attr(fit, "ild_ar1", exact = TRUE)) || has_cor,
    class = if (!is.null(corr_class)) as.character(corr_class)[1L] else NA_character_,
    coef_corStruct = coef_cs
  )
  out$heterogeneity <- fill_diagnostics_heterogeneity(fit)
  out
}

#' Fit diagnostics -- brms (uses attr ild_posterior)
#' @keywords internal
#' @noRd
fill_diagnostics_fit_brms <- function(fit) {
  ps <- attr(fit, "ild_posterior", exact = TRUE)
  if (is.null(ps)) ps <- ild_posterior_attr(fit, prior_template = NULL)
  conv_tbl <- NULL
  sf <- tryCatch(summary(fit)$fixed, error = function(e) NULL)
  if (!is.null(sf) && nrow(sf) > 0L) {
    conv_tbl <- tibble::tibble(
      term = rownames(sf),
      rhat = as.numeric(sf[["Rhat"]]),
      ess_bulk = as.numeric(sf[["Bulk_ESS"]]),
      ess_tail = as.numeric(sf[["Tail_ESS"]])
    )
  }
  mx <- ild_brms_max_rhat(fit)
  out <- list(
    engine = "brms",
    convergence = list(
      convergence_table = conv_tbl,
      max_rhat = mx
    ),
    sampler = list(
      n_divergent = ps$n_divergent,
      n_max_treedepth_hits = ps$n_max_treedepth_hits,
      chains = ps$chains,
      iter = ps$iter,
      warmup = ps$warmup
    ),
    residual_correlation = list(
      modeled = NA,
      note = "See brms family/residual structure and posterior predictive checks for residual behavior."
    ),
    posterior_summary = list(ild_posterior = ps)
  )
  out$heterogeneity <- fill_diagnostics_heterogeneity(fit)
  out
}

#' Residual diagnostics -- frequentist (embeds legacy [ild_diagnostics()] for plotting)
#' @keywords internal
#' @noRd
fill_diagnostics_residual_frequentist <- function(object, data, type, by_id) {
  diag <- ild_diagnostics(object, data = data, type = type, by_id = by_id)
  aug <- tryCatch(augment_ild_model(object), error = function(e) NULL)
  res_sd <- NA_real_
  cor_obs_fitted <- NA_real_
  if (!is.null(aug) && nrow(aug) > 0L) {
    res_sd <- stats::sd(aug$.resid, na.rm = TRUE)
    if (all(is.finite(aug$.fitted)) && stats::sd(aug$.fitted, na.rm = TRUE) > 0) {
      cor_obs_fitted <- stats::cor(aug$.outcome, aug$.fitted, use = "pairwise.complete.obs")
    }
  }
  list(
    engine = if (inherits(object, "lme")) "lme" else "lmer",
    legacy_ild_diagnostics = diag,
    residual_sd = res_sd,
    cor_observed_fitted = cor_obs_fitted,
    stats = diag$stats,
    meta = diag$meta
  )
}

#' Residual layer--  brms (lighter; legacy ACF not computed here)
#' @keywords internal
#' @noRd
fill_diagnostics_residual_brms <- function(fit) {
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  out <- list(engine = "brms", legacy_ild_diagnostics = NULL, residual_sd = NA_real_)
  if (!is.null(aug) && ".resid" %in% names(aug)) {
    out$residual_sd <- stats::sd(aug$.resid, na.rm = TRUE)
    out$mean_abs_resid <- mean(abs(aug$.resid), na.rm = TRUE)
  }
  out
}

#' Predictive layer -- frequentist (observed vs fitted summaries)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_frequentist <- function(object, data) {
  aug <- tryCatch(augment_ild_model(object), error = function(e) NULL)
  if (is.null(aug)) return(NULL)
  om <- fill_diagnostics_predictive_obs_metrics(aug)
  eng <- if (inherits(object, "lme")) "lme" else "lmer"
  c(
    list(
      engine = eng,
      obs_metrics = om,
      simulation_checks = list(ppc = NULL)
    ),
    om
  )
}

#' @keywords internal
#' @noRd
fill_diagnostics_predictive_obs_metrics <- function(aug) {
  if (is.null(aug) || !all(c(".outcome", ".fitted") %in% names(aug))) {
    return(list())
  }
  y <- aug$.outcome
  f <- aug$.fitted
  ok <- is.finite(y) & is.finite(f)
  n <- sum(ok)
  ye <- y[ok]
  fe <- f[ok]
  res <- ye - fe
  list(
    n = as.integer(n),
    mean_abs_error = if (n > 0L) mean(abs(res), na.rm = TRUE) else NA_real_,
    rmse = if (n > 0L) sqrt(mean(res^2, na.rm = TRUE)) else NA_real_,
    mean_residual = if (n > 0L) mean(res, na.rm = TRUE) else NA_real_,
    max_abs_error = if (n > 0L) max(abs(res), na.rm = TRUE) else NA_real_,
    cor_observed_fitted = if (n > 1L) stats::cor(ye, fe, use = "pairwise.complete.obs") else NA_real_
  )
}

#' Predictive layer -- brms augment-only (no PPC)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_brms_augment_only <- function(fit) {
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  om <- fill_diagnostics_predictive_obs_metrics(aug)
  c(
    list(
      engine = "brms",
      obs_metrics = om,
      simulation_checks = list(ppc = NULL)
    ),
    om
  )
}

#' Predictive layer -- brms (PPC summary)
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_brms <- function(fit, ppc_ndraws) {
  ppc <- ild_brms_ppc_summary(fit, ndraws = ppc_ndraws)
  aug <- tryCatch(ild_augment(fit), error = function(e) NULL)
  om <- fill_diagnostics_predictive_obs_metrics(aug)
  c(
    list(
      engine = "brms",
      obs_metrics = om,
      simulation_checks = list(ppc = ppc),
      ppc = ppc
    ),
    om
  )
}

#' Fit layer -- ctsem
#' @keywords internal
#' @noRd
fill_diagnostics_fit_ctsem <- function(object) {
  fit <- object$ct_fit
  conv <- tryCatch(fit$optimization$convergence, error = function(e) NA_integer_)
  # ctsem/OpenMx may expose empty-length convergence slots on some platforms; guard
  # so `if (!is.finite(conv))` never sees logical(0).
  if (length(conv) != 1L || !is.finite(conv)) {
    conv <- tryCatch(fit$opt$convergence, error = function(e) NA_integer_)
  }
  if (length(conv) != 1L || !is.finite(conv)) {
    conv <- NA_integer_
  }
  ll <- tryCatch(as.numeric(stats::logLik(fit)[1L]), error = function(e) NA_real_)
  drift_abs_max <- tryCatch({
    dr <- suppressWarnings(as.numeric(fit$DRIFT))
    dr <- dr[is.finite(dr)]
    if (length(dr) == 0L) NA_real_ else max(abs(dr))
  }, error = function(e) NA_real_)
  list(
    engine = "ctsem",
    convergence = list(
      converged = if (is.finite(conv)) isTRUE(conv == 0L) else .ild_ctsem_is_converged(object),
      convergence_code = conv,
      note = "Convergence extracted from ctsem optimization slots when available."
    ),
    optimizer = list(
      note = "See ct_fit object for optimizer details."
    ),
    log_likelihood = ll,
    state_dimension = tryCatch(as.integer(ncol(fit$DRIFT)), error = function(e) NA_integer_),
    drift_abs_max = drift_abs_max,
    residual_correlation = list(
      modeled = TRUE,
      structure = "continuous_time_state_dynamics",
      note = "Temporal dependence is represented through continuous-time latent dynamics."
    )
  )
}

#' Residual layer -- ctsem
#' @keywords internal
#' @noRd
fill_diagnostics_residual_ctsem <- function(object, data) {
  aug <- tryCatch(ild_augment(object), error = function(e) NULL)
  if (is.null(aug)) {
    return(list(engine = "ctsem", legacy_ild_diagnostics = NULL))
  }
  acf_obj <- tryCatch(stats::acf(aug$.resid, plot = FALSE, na.action = stats::na.pass), error = function(e) NULL)
  acf_tbl <- if (!is.null(acf_obj)) {
    tibble::tibble(lag = as.numeric(acf_obj$lag), acf = as.numeric(acf_obj$acf))
  } else {
    tibble::tibble(lag = numeric(), acf = numeric())
  }
  qq_cor <- tryCatch(
    stats::cor(
      stats::qnorm(stats::ppoints(sum(is.finite(aug$.resid_std)))),
      sort(aug$.resid_std[is.finite(aug$.resid_std)])
    ),
    error = function(e) NA_real_
  )
  list(
    engine = "ctsem",
    residual_sd = stats::sd(aug$.resid, na.rm = TRUE),
    acf = acf_tbl,
    qq = list(correlation = qq_cor),
    legacy_ild_diagnostics = NULL
  )
}

#' Predictive layer -- ctsem
#' @keywords internal
#' @noRd
fill_diagnostics_predictive_ctsem <- function(object) {
  aug <- tryCatch(ild_augment(object), error = function(e) NULL)
  if (is.null(aug)) {
    return(list(engine = "ctsem", obs_metrics = NULL, simulation_checks = list(ppc = NULL)))
  }
  om <- fill_diagnostics_predictive_obs_metrics(aug)
  c(
    list(
      engine = "ctsem",
      obs_metrics = om,
      simulation_checks = list(ppc = NULL)
    ),
    om
  )
}

#' Missingness-focused slice (formula / model variables when available)
#' @keywords internal
#' @noRd
fill_diagnostics_missingness_section <- function(data, vars = NULL) {
  validate_ild(data)
  if (is.null(vars) || length(vars) == 0L) {
    return(list(
      note = "No model variables supplied; see data$missing_pattern for global missingness.",
      summary = NULL,
      overall = NULL,
      pct_na_by_var = NULL
    ))
  }
  mp <- tryCatch(ild_missing_pattern(data, vars = vars), error = function(e) NULL)
  if (is.null(mp)) {
    return(list(
      note = NULL,
      summary = NULL,
      overall = NULL,
      pct_na_by_var = NULL
    ))
  }
  pct_na_by_var <- NULL
  if (!is.null(mp$summary) && nrow(mp$summary) > 0L && "pct_na" %in% names(mp$summary)) {
    pct_na_by_var <- mp$summary[, intersect(c("variable", "pct_na"), names(mp$summary)), drop = FALSE]
  }
  list(
    note = NULL,
    summary = mp$summary,
    overall = mp$overall,
    pct_na_by_var = pct_na_by_var
  )
}

#' Causal / IPW slice when weight columns present
#' @keywords internal
#' @noRd
fill_diagnostics_causal <- function(data,
                                    causal_detail = FALSE,
                                    balance = FALSE,
                                    balance_treatment = NULL,
                                    balance_covariates = NULL,
                                    balance_weights_col = ".ipw_treat",
                                    balance_by_occasion = FALSE) {
  if (is.null(data) || !is_ild(data)) return(NULL)
  wcols <- grep("^\\.ipw|ipw_weight|\\.w_", names(data), value = TRUE, ignore.case = TRUE)
  if (length(wcols) == 0L && !isTRUE(balance)) return(NULL)
  out <- list(columns_found = wcols)
  .summ_w <- function(w) {
    list(
      min = min(w, na.rm = TRUE),
      max = max(w, na.rm = TRUE),
      mean = mean(w, na.rm = TRUE)
    )
  }
  if (".ipw" %in% names(data)) {
    w <- data[[".ipw"]]
    out$weight_summary <- .summ_w(w)
    if (isTRUE(causal_detail)) {
      wf <- w[is.finite(w)]
      if (length(wf) > 0L) {
        qs <- stats::quantile(wf, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, names = TRUE)
        out$weight_detail <- list(
          quantiles = qs,
          sum_w = sum(wf, na.rm = TRUE)
        )
      }
    }
  }
  if (".ipw_treat" %in% names(data)) {
    out$weight_summary_treat <- .summ_w(data[[".ipw_treat"]])
    if (isTRUE(causal_detail)) {
      wf <- data[[".ipw_treat"]][is.finite(data[[".ipw_treat"]])]
      if (length(wf) > 0L) {
        out$weight_detail_treat <- list(
          quantiles = stats::quantile(wf, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, names = TRUE)
        )
      }
    }
  }
  if (".ipw_censor" %in% names(data)) {
    out$weight_summary_censor <- .summ_w(data[[".ipw_censor"]])
    if (isTRUE(causal_detail)) {
      wf <- data[[".ipw_censor"]][is.finite(data[[".ipw_censor"]])]
      if (length(wf) > 0L) {
        out$weight_detail_censor <- list(
          quantiles = stats::quantile(wf, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, names = TRUE)
        )
      }
    }
  }
  if (isTRUE(causal_detail) && ".ipw_treat" %in% names(data) && ".ild_seq" %in% names(data) &&
      !is.null(attr(data, "ild_iptw_msm_fits", exact = TRUE))) {
    wt <- data[[".ipw_treat"]]
    sq <- data[[".ild_seq"]]
    uq <- sort(unique(as.numeric(sq)))
    occ_summ <- vector("list", length(uq))
    names(occ_summ) <- as.character(uq)
    for (tt in uq) {
      wm <- wt[as.numeric(sq) == tt & is.finite(wt)]
      if (length(wm) > 0L) {
        occ_summ[[as.character(tt)]] <- list(
          min = min(wm, na.rm = TRUE),
          max = max(wm, na.rm = TRUE),
          mean = mean(wm, na.rm = TRUE)
        )
      }
    }
    out$msm_treat_weight_by_occasion <- occ_summ
  }
  if (isTRUE(balance) && length(balance_covariates) > 0L && !is.null(balance_treatment) &&
      balance_treatment %in% names(data) && balance_weights_col %in% names(data)) {
    out$balance <- list(
      table = ild_msm_balance(
        data,
        treatment = balance_treatment,
        covariates = balance_covariates,
        weights_col = balance_weights_col,
        by_occasion = balance_by_occasion
      ),
      ess = ild_ipw_ess(
        data,
        weights_col = balance_weights_col,
        by_occasion = balance_by_occasion
      )
    )
  }
  out
}

#' Build warnings tibble
#' @param rows List of named lists with source, level, message, code
#' @keywords internal
#' @noRd
collect_diagnostics_warnings <- function(rows = list()) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      source = character(),
      level = character(),
      message = character(),
      code = character()
    ))
  }
  tibble::tibble(
    source = vapply(rows, function(z) .fld(z, "source"), character(1)),
    level = vapply(rows, function(z) .fld(z, "level"), character(1)),
    message = vapply(rows, function(z) .fld(z, "message"), character(1)),
    code = vapply(rows, function(z) .fld(z, "code"), character(1))
  )
}

#' @keywords internal
#' @noRd
.fld <- function(z, nm) {
  v <- z[[nm]]
  if (is.null(v)) NA_character_ else as.character(v)[1]
}

#' Build guardrails tibble (delegates to registry; uses \code{rule_id} schema)
#' @keywords internal
#' @noRd
collect_diagnostics_guardrails <- function(rows = list()) {
  guardrail_finalize_rows(rows)
}

.fld2 <- function(z, nm) {
  v <- z[[nm]]
  if (is.null(v)) NA_character_ else paste(as.character(v), collapse = " ")
}

#' Assemble warnings for frequentist fits (guardrails via \code{\link{evaluate_guardrails_fit}})
#' @keywords internal
#' @noRd
collect_freq_warnings_guardrails <- function(fit, fit_diag) {
  warns <- list()
  if (inherits(fit, "lmerMod") && length(fit@optinfo$warnings) > 0L) {
    for (i in seq_along(fit@optinfo$warnings)) {
      warns[[length(warns) + 1L]] <- list(
        source = "lme4",
        level = "warning",
        message = conditionMessage(fit@optinfo$warnings[[i]]),
        code = "lmer_warning"
      )
    }
  }
  list(
    warnings = collect_diagnostics_warnings(warns),
    guardrails = guardrails_empty_tibble()
  )
}

#' @keywords internal
#' @noRd
collect_brms_warnings_guardrails <- function(fit, fit_diag) {
  warns <- list()
  ps <- fit_diag$posterior_summary$ild_posterior %||% fit_diag$ild_posterior
  if (!is.null(ps) && !is.na(ps$n_divergent) && ps$n_divergent > 0L) {
    warns[[length(warns) + 1L]] <- list(
      source = "stan",
      level = "warning",
      message = sprintf("%s divergent transition(s) after warmup.", ps$n_divergent),
      code = "divergent_transitions"
    )
  }
  list(
    warnings = collect_diagnostics_warnings(warns),
    guardrails = guardrails_empty_tibble()
  )
}

#' Collapse bundle into short narrative strings
#' @keywords internal
#' @noRd
build_diagnostics_bundle_summary <- function(bundle) {
  parts <- character()
  if (!is.null(bundle$meta$engine)) {
    parts <- c(parts, sprintf("Engine: %s.", bundle$meta$engine))
  }
  if (!is.null(bundle$fit)) {
    ff <- bundle$fit
    sing <- ff$convergence$singular %||% ff$singular
    if (!is.null(sing) && !is.na(sing) && isTRUE(sing)) {
      parts <- c(parts, "Singular / problematic fit variance structure reported.")
    }
    xrk <- ff$rank$fixed_effects_X %||% ff$X_rank
    if (!is.null(xrk) && !is.na(xrk)) {
      parts <- c(parts, sprintf("Fixed-effects design rank (X): %s.", xrk))
    }
    mr <- ff$convergence$max_rhat %||% ff$max_rhat
    if (!is.null(mr) && is.finite(mr)) {
      parts <- c(parts, sprintf("Max R-hat: %.3f.", mr))
    }
  }
  if (!is.null(bundle$data$obs_per_id)) {
    opi <- bundle$data$obs_per_id
    if (!is.null(opi$min) && !is.null(opi$max)) {
      parts <- c(parts, sprintf(
        "Observations per person: min = %s, median = %.1f, max = %s.",
        opi$min, opi$median, opi$max
      ))
    }
  }
  if (!is.null(bundle$data$outcome_summaries) && nrow(bundle$data$outcome_summaries) > 0L &&
        "pct_na" %in% names(bundle$data$outcome_summaries)) {
    pm <- suppressWarnings(max(bundle$data$outcome_summaries$pct_na, na.rm = TRUE))
    if (is.finite(pm) && pm > 0) {
      parts <- c(parts, sprintf("Outcome(s) missingness up to %.1f%% (see data$outcome_summaries).", pm))
    }
  }
  if (!is.null(bundle$residual$legacy_ild_diagnostics)) {
    d <- bundle$residual$legacy_ild_diagnostics
    parts <- c(parts, sprintf(
      "Residual diagnostics (%s): n_obs = %s.",
      d$meta$engine, d$meta$n_obs
    ))
  }
  ppc_raw <- bundle$predictive$simulation_checks$ppc %||% bundle$predictive$ppc
  if (!is.null(bundle$predictive) && is.list(ppc_raw) && is.null(ppc_raw$error)) {
    p <- ppc_raw
    parts <- c(parts, sprintf(
      "PPC: |mean_obs - mean_rep| = %.4f; |sd_obs - sd_rep| = %.4f.",
      p$mean_abs_diff, p$sd_abs_diff
    ))
  }
  if (nrow(bundle$warnings) > 0L) {
    parts <- c(parts, sprintf("%d diagnostic warning(s) recorded.", nrow(bundle$warnings)))
  }
  if (nrow(bundle$guardrails) > 0L) {
    parts <- c(parts, sprintf("%d guardrail(s) triggered (see guardrail_registry).", nrow(bundle$guardrails)))
  }
  parts
}
