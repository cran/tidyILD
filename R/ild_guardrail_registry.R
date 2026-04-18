# Guardrail registry and evaluation for ild_diagnostics_bundle$guardrails
# Schema: rule_id, section, severity, triggered, message, recommendation

#' @importFrom tibble tibble
NULL

ILD_GUARDRAIL_REGISTRY <- tibble::tibble(
  rule_id = c(
    "GR_SINGULAR_RANDOM_EFFECTS",
    "GR_POOR_POSTERIOR_MIXING",
    "GR_LOW_ESS",
    "GR_MIXED_PREDICTOR_NOT_DECOMPOSED",
    "GR_IRREGULAR_SPACING_NO_RESID_COR",
    "GR_HIGH_TIMING_GAP_RATE",
    "GR_DROPOUT_LATE_CONCENTRATION",
    "GR_IPW_WEIGHTS_UNSTABLE",
    "GR_MSM_COMPONENT_WEIGHTS_UNSTABLE",
    "GR_MSM_BALANCE_SMD_HIGH",
    "GR_MSM_ESS_LOW",
    "GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME",
    "GR_KFAS_SHORT_SERIES_FOR_STATE_SPACE",
    "GR_KFAS_STATE_DIMENSION_HIGH_FOR_N",
    "GR_KFAS_DEGENERATE_VARIANCE_ESTIMATE",
    "GR_KFAS_NONCONVERGENCE",
    "GR_KFAS_MANY_MISSING_OUTCOME_SEGMENTS",
    "GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY",
    "GR_CTSEM_NONCONVERGENCE",
    "GR_CTSEM_UNSTABLE_DYNAMICS",
    "GR_CTSEM_SHORT_SERIES_FOR_COMPLEX_DYNAMICS",
    "GR_RE_SLOPE_VARIANCE_VERSUS_RESIDUAL_LOW",
    "GR_PERSON_SPECIFIC_SLOPES_EMPIRICALLY_TIGHT",
    "GR_LAG_MEAN_STRONG_RESIDUAL_ACF_NO_AR",
    "GR_INDEX_LAG_IRREGULAR_SPACING"
  ),
  section = c(
    "fit", "fit", "fit",
    "design", "data", "data",
    "missingness", "causal", "causal", "causal", "causal",
    "design", "data", "fit", "fit", "fit", "data", "fit",
    "fit", "fit", "data",
    "fit", "fit",
    "residual", "data"
  ),
  severity = c(
    "warning", "warning", "warning",
    "warning", "warning", "info",
    "warning", "warning", "warning", "info", "info",
    "warning", "warning", "warning", "warning", "warning", "info", "warning",
    "warning", "warning", "warning",
    "info", "info",
    "warning", "info"
  ),
  default_message = c(
    "Estimated random-effect covariance is singular or near-singular.",
    "Max R-hat exceeds a conventional mixing threshold.",
    "Bulk effective sample size is low for at least one fixed-effect parameter.",
    "Predictor(s) vary within and between persons but are not WP/BP decomposed.",
    "Irregular spacing is present but residual temporal correlation (AR1/CAR1) is not modeled.",
    "A large fraction of time intervals are flagged as gaps.",
    "Missingness on the outcome is more concentrated in the later part of the study timeline.",
    "Inverse probability weights show extreme variability (possible instability).",
    "IPTW or IPCW component weights show extreme variability (possible instability).",
    "At least one covariate shows large weighted standardized mean difference after weighting.",
    "Effective sample size from the analysis weights is low relative to the number of rows.",
    "Interval timing is highly irregular relative to the median step; discrete-time KFAS models assume a consistent time index.",
    "The observed series is short for the requested state-space complexity.",
    "Latent state dimension is large relative to series length.",
    "Estimated state or observation variance is effectively on the boundary (near zero or degenerate).",
    "Numerical optimization for the state-space model did not report clean convergence.",
    "The outcome has many separate missing segments in time order.",
    "Independent single-subject state-space fits are not a substitute for pooled multilevel latent-dynamics models (e.g. ctsem).",
    "ctsem optimization did not report clean convergence.",
    "Estimated continuous-time dynamics appear unstable or implausibly large.",
    "The observed series may be too short for the requested continuous-time latent dynamics complexity.",
    "Random slope standard deviation is small relative to residual standard deviation.",
    "Person-specific partial-pooling slopes show very little empirical spread versus residual noise.",
    "Pooled residual ACF at lag 1 is large while the mean includes lag columns and residual AR1/CAR1 is not modeled.",
    "Index-based ild_lag() was used on irregular-ish spacing; row-order lags may not align with elapsed time between observations."
  ),
  default_recommendation = c(
    "Consider simpler random structure, stronger priors (Bayesian), or rescaling predictors.",
    "Run longer chains, reparameterize, or tighten priors.",
    "Increase iterations or use stronger priors; interpret posterior means with caution.",
    "Use ild_center() to create _wp and _bp components to avoid conflation bias.",
    "Consider ild_lme(..., ar1 = TRUE) or interpret residual autocorrelation diagnostics cautiously.",
    "Review gap_threshold and missingness; lags and spacing-based assumptions may be strained.",
    "Investigate selective dropout (MNAR) and sensitivity analyses; consider IPW or models for missingness.",
    "Review the missingness model, trim bounds, or stabilize weights before interpreting results.",
    "Review treatment and censoring models, trim bounds, or stabilize weights before interpreting MSM results.",
    "Review confounder selection, functional forms, overlap, or weight stabilization.",
    "Consider trimming, stabilizing, or respecifying weight models; interpret weighted estimates cautiously.",
    "Align or aggregate to a regular grid, or use continuous-time modeling for irregular spacing.",
    "Collect more time points or simplify the state specification (e.g. fewer latent components).",
    "Simplify the model (fewer states) or extend the series before interpreting complex latent structure.",
    "Rescale the outcome, re-check identification, or consider stronger priors / constraints in KFAS.",
    "Try different starting values, optimization method, or check the model specification.",
    "Consider explicit missingness modeling or sensitivity analyses for intermittent observations.",
    "Do not treat stacked single-ID KFAS fits as a pooled latent model; use hierarchical latent frameworks if that is the estimand.",
    "Try alternative starting values, simplify the ctsem model, or inspect time scaling and identification.",
    "Rescale time/outcome or constrain unstable drift parameters before interpretation.",
    "Collect more observations per person or simplify latent dynamics before strong inferences.",
    "Use ild_heterogeneity() and VarCorr(); consider a simpler random structure if slopes are not substantively heterogeneous.",
    "Compare to ild_person_model() and population fixed effects; confirm whether random slopes merit interpretation.",
    "Consider ild_lme(..., ar1 = TRUE) or a dynamics model that matches spacing; see vignette(\"temporal-dynamics-model-choice\", package = \"tidyILD\").",
    "Prefer ild_lag(..., mode = \"gap_aware\" or \"time_window\"), align to a grid, or use continuous-time models when appropriate."
  )
)

#' Guardrail rule registry (analysis safety layer)
#'
#' Returns the canonical catalog of \strong{guardrail} rules tidyILD may use. Each rule has a
#' stable \code{rule_id}, \code{section} (bundle slot), \code{severity}, and default
#' \code{default_message} / \code{default_recommendation} text. When you call
#' \code{\link{ild_diagnose}}, \strong{triggered} rules appear as rows in
#' \code{\link{ild_diagnostics_bundle}$guardrails} with \code{message} and
#' \code{recommendation} possibly customized for the run.
#'
#' @return A tibble with columns \code{rule_id}, \code{section}, \code{severity},
#'   \code{default_message}, \code{default_recommendation}.
#' @seealso \code{\link{ild_diagnose}}, \code{\link{ild_diagnostics_bundle}}
#' @export
guardrail_registry <- function() {
  ILD_GUARDRAIL_REGISTRY
}

#' @keywords internal
#' @noRd
guardrail_registry_lookup <- function(rule_id) {
  r <- ILD_GUARDRAIL_REGISTRY
  m <- match(rule_id, r$rule_id)
  if (is.na(m)) {
    return(list(
      section = "unknown",
      severity = "info",
      default_message = "",
      default_recommendation = ""
    ))
  }
  list(
    section = r$section[m],
    severity = r$severity[m],
    default_message = r$default_message[m],
    default_recommendation = r$default_recommendation[m]
  )
}

#' Numeric rank for guardrail severity (higher = stronger)
#'
#' \code{info} < \code{warning}; unknown levels rank lowest (0).
#' @param severity Character vector of severity labels.
#' @return Integer vector of same length as \code{severity}.
#' @keywords internal
#' @noRd
guardrail_severity_rank <- function(severity) {
  s <- tolower(as.character(severity))
  ifelse(s == "warning", 2L, ifelse(s == "info", 1L, 0L))
}

#' Highest severity among triggered guardrails
#'
#' @param severities Character vector (e.g. from \code{guardrails$severity}).
#' @return Single severity string (\code{"warning"} or \code{"info"}) or
#'   \code{NA_character_} if empty.
#' @keywords internal
#' @noRd
guardrail_max_severity <- function(severities) {
  if (length(severities) == 0L) {
    return(NA_character_)
  }
  sev <- as.character(severities)
  r <- guardrail_severity_rank(sev)
  sev[which.max(r)[1]]
}

#' Structured summary and short narrative for guardrails tibble
#'
#' Used by \code{\link{print.ild_diagnostics_bundle}}, \code{\link{ild_report}},
#' and \code{\link{ild_methods}}. Does not include full \code{message} text.
#'
#' @param guardrails A guardrails tibble (e.g. \code{bundle$guardrails}), or an
#'   \code{\link{ild_diagnostics_bundle}} (uses \code{$guardrails}).
#' @param max_rule_ids Maximum unique \code{rule_id} values to list in the narrative (default 5).
#' @return A list: \code{n} (integer), \code{max_severity} (character or \code{NA}),
#'   \code{rule_ids} (character, unique, order preserved), \code{narrative} (single string, may be empty).
#' @keywords internal
#' @noRd
ild_guardrails_summary <- function(guardrails, max_rule_ids = 5L) {
  if (inherits(guardrails, "ild_diagnostics_bundle")) {
    guardrails <- guardrails$guardrails
  }
  if (is.null(guardrails) || !tibble::is_tibble(guardrails) || nrow(guardrails) == 0L) {
    return(list(
      n = 0L,
      max_severity = NA_character_,
      rule_ids = character(0),
      narrative = ""
    ))
  }
  n <- nrow(guardrails)
  max_sev <- guardrail_max_severity(guardrails$severity)
  rids <- unique(as.character(guardrails$rule_id))
  show_ids <- rids[seq_len(min(length(rids), as.integer(max_rule_ids)))]
  id_part <- if (length(show_ids) > 0L) {
    paste(show_ids, collapse = ", ")
  } else {
    ""
  }
  sev_part <- if (!is.na(max_sev) && nzchar(max_sev)) {
    sprintf("max severity %s", max_sev)
  } else {
    ""
  }
  narrative <- if (n > 0L) {
    paste0(
      sprintf("%d methodological guardrail%s triggered", n, if (n == 1L) "" else "s"),
      if (nzchar(sev_part)) paste0(" (", sev_part, ")") else "",
      if (nzchar(id_part)) paste0(": ", id_part) else "",
      "."
    )
  } else {
    ""
  }
  list(
    n = as.integer(n),
    max_severity = max_sev,
    rule_ids = rids,
    narrative = narrative
  )
}

#' Empty guardrails tibble (canonical schema)
#' @keywords internal
#' @noRd
guardrails_empty_tibble <- function() {
  tibble::tibble(
    rule_id = character(),
    section = character(),
    severity = character(),
    triggered = logical(),
    message = character(),
    recommendation = character()
  )
}

#' Assemble guardrails from evaluated rows (rule_id + trigger + optional overrides)
#' @keywords internal
#' @noRd
guardrail_finalize_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(guardrails_empty_tibble())
  }
  out_rule <- character()
  out_sec <- character()
  out_sev <- character()
  out_trig <- logical()
  out_msg <- character()
  out_rec <- character()
  for (z in rows) {
    if (!isTRUE(z$triggered)) next
    rid <- as.character(z$rule_id)[1]
    def <- guardrail_registry_lookup(rid)
    msg <- z$message
    if (is.null(msg) || (length(msg) == 1L && !nzchar(msg))) {
      msg <- def$default_message
    } else {
      msg <- as.character(msg)[1]
    }
    rec <- z$recommendation
    if (is.null(rec) || (length(rec) == 1L && !nzchar(rec))) {
      rec <- def$default_recommendation
    } else {
      rec <- as.character(rec)[1]
    }
    out_rule <- c(out_rule, rid)
    out_sec <- c(out_sec, def$section)
    out_sev <- c(out_sev, def$severity)
    out_trig <- c(out_trig, TRUE)
    out_msg <- c(out_msg, msg)
    out_rec <- c(out_rec, rec)
  }
  tibble::tibble(
    rule_id = out_rule,
    section = out_sec,
    severity = out_sev,
    triggered = out_trig,
    message = out_msg,
    recommendation = out_rec
  )
}

#' @keywords internal
#' @noRd
ild_formula_includes_lag_terms <- function(f) {
  if (is.null(f)) {
    return(FALSE)
  }
  rhs <- tryCatch(stats::terms(f), error = function(e) NULL)
  if (is.null(rhs)) {
    return(FALSE)
  }
  labels <- attr(rhs, "term.labels")
  any(grepl("_lag[0-9]+$|_lag_window$", labels))
}

#' @keywords internal
#' @noRd
ild_bundle_residual_acf_lag1 <- function(bundle) {
  ac <- tryCatch(bundle$residual$stats$acf$pooled, error = function(e) NULL)
  if (is.null(ac) || nrow(ac) == 0L) {
    return(NA_real_)
  }
  hit <- ac[ac$lag %in% c(1, 1L), , drop = FALSE]
  if (nrow(hit) == 0L) {
    return(NA_real_)
  }
  as.numeric(hit$acf[1L])
}

#' @keywords internal
#' @noRd
ild_provenance_has_index_lag <- function(data) {
  p <- ild_get_history(data)
  if (is.null(p) || is.null(p$steps)) {
    return(FALSE)
  }
  for (s in p$steps) {
    if (identical(as.character(s$step), "ild_lag")) {
      am <- s$args$mode
      if (identical(am, "index")) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Evaluate design / data / causal guardrails from bundle + model
#' @keywords internal
#' @noRd
evaluate_guardrails_contextual <- function(object, data, bundle, engine = c("lmer", "lme", "brms", "ctsem")) {
  engine <- match.arg(engine)
  rows <- list()
  if (!is.null(data) && is_ild(data)) {
    f <- tryCatch(stats::formula(object), error = function(e) NULL)
    if (!is.null(f)) {
      unc <- ild_detect_uncentered_predictors(data, f)
      if (length(unc) > 0L) {
        rows[[length(rows) + 1L]] <- list(
          rule_id = "GR_MIXED_PREDICTOR_NOT_DECOMPOSED",
          triggered = TRUE,
          message = sprintf(
            "Predictor(s) %s vary within and between persons but are not WP/BP decomposed.",
            paste(paste0("'", unc, "'"), collapse = ", ")
          ),
          recommendation = NULL
        )
      }
    }
    if (engine %in% c("lmer", "lme")) {
      ar1_dyn <- tryCatch(isTRUE(attr(object, "ild_ar1", exact = TRUE)), error = function(e) FALSE)
      if (!ar1_dyn && !is.null(f) && ild_formula_includes_lag_terms(f)) {
        acf1 <- ild_bundle_residual_acf_lag1(bundle)
        if (is.finite(acf1) && acf1 > 0.2) {
          rows[[length(rows) + 1L]] <- list(
            rule_id = "GR_LAG_MEAN_STRONG_RESIDUAL_ACF_NO_AR",
            triggered = TRUE,
            message = sprintf(
              "Pooled residual ACF at lag 1 is %.2f with lag term(s) in the mean and residual AR1/CAR1 not used.",
              acf1
            ),
            recommendation = NULL
          )
        }
      }
      sc2 <- tryCatch(ild_spacing_class(data), error = function(e) NA_character_)
      if (!is.na(sc2) && sc2 == "irregular-ish" && ild_provenance_has_index_lag(data)) {
        rows[[length(rows) + 1L]] <- list(
          rule_id = "GR_INDEX_LAG_IRREGULAR_SPACING",
          triggered = TRUE,
          message = NULL,
          recommendation = NULL
        )
      }
    }
    ar1 <- tryCatch(isTRUE(attr(object, "ild_ar1", exact = TRUE)), error = function(e) FALSE)
    sc <- tryCatch(ild_spacing_class(data), error = function(e) NA_character_)
    if (!is.na(sc) && sc == "irregular-ish" && !ar1) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_IRREGULAR_SPACING_NO_RESID_COR",
        triggered = TRUE,
        message = NULL,
        recommendation = NULL
      )
    }
    pg <- if (!is.null(bundle$data)) bundle$data$pct_gap else NA_real_
    if (is.na(pg)) {
      pg <- tryCatch(ild_summary(data)$pct_gap, error = function(e) NA_real_)
    }
    if (is.finite(pg) && pg > 25) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_HIGH_TIMING_GAP_RATE",
        triggered = TRUE,
        message = sprintf(
          "About %.1f%% of intervals are gaps (threshold for rule > 25%%).",
          pg
        ),
        recommendation = NULL
      )
    }
    rn <- tryCatch(ild_response_name(object), error = function(e) NULL)
    if (!is.null(rn) && rn %in% names(data)) {
      if (guardrail_dropout_late_heuristic(data, rn)) {
        rows[[length(rows) + 1L]] <- list(
          rule_id = "GR_DROPOUT_LATE_CONCENTRATION",
          triggered = TRUE,
          message = sprintf(
            "Outcome '%s' is missing more often in the later half of the pooled timeline than in the earlier half.",
            rn
          ),
          recommendation = NULL
        )
      }
    }
  }
  if (!is.null(bundle$causal) && is.list(bundle$causal$weight_summary)) {
    ws <- bundle$causal$weight_summary
    mx <- ws$max
    mn <- ws$min
    if (is.finite(mx) && is.finite(mn) && mn > 0 && (mx / mn) > 50) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_IPW_WEIGHTS_UNSTABLE",
        triggered = TRUE,
        message = sprintf("IPW weight range ratio max/min = %.1f (threshold > 50).", mx / mn),
        recommendation = NULL
      )
    }
  }
  if (!is.null(data) && is_ild(data)) {
    .wm_ratio <- function(w) {
      wf <- w[is.finite(w) & w > 0]
      if (length(wf) < 2L) return(NA_real_)
      max(wf) / min(wf)
    }
    for (cn in c(".ipw_treat", ".ipw_censor")) {
      if (cn %in% names(data)) {
        r <- .wm_ratio(data[[cn]])
        if (is.finite(r) && r > 50) {
          rows[[length(rows) + 1L]] <- list(
            rule_id = "GR_MSM_COMPONENT_WEIGHTS_UNSTABLE",
            triggered = TRUE,
            message = sprintf(
              "Component %s weight range ratio max/min = %.1f (threshold > 50).",
              cn, r
            ),
            recommendation = NULL
          )
        }
      }
    }
  }
  if (!is.null(bundle$causal) && is.list(bundle$causal$balance) && !is.null(bundle$causal$balance$table)) {
    tab <- bundle$causal$balance$table
    if (inherits(tab, "tbl_df") && nrow(tab) > 0L) {
      mx <- suppressWarnings(max(abs(tab$smd), na.rm = TRUE))
      if (is.finite(mx) && mx > 0.25) {
        rows[[length(rows) + 1L]] <- list(
          rule_id = "GR_MSM_BALANCE_SMD_HIGH",
          triggered = TRUE,
          message = sprintf("Maximum |SMD| across balance table = %.3f (threshold > 0.25).", mx),
          recommendation = NULL
        )
      }
    }
    ess <- bundle$causal$balance$ess
    ess_min <- if (inherits(ess, "tbl_df")) {
      suppressWarnings(min(ess$ess, na.rm = TRUE))
    } else {
      as.numeric(ess)[1L]
    }
    n <- if (!is.null(data) && is_ild(data)) nrow(data) else NA_integer_
    if (is.finite(ess_min) && is.finite(n) && ess_min < max(30, 0.05 * n)) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_MSM_ESS_LOW",
        triggered = TRUE,
        message = sprintf(
          "Minimum ESS = %.1f (threshold: max(30, 0.05 * N) with N = %d).",
          ess_min, n
        ),
        recommendation = NULL
      )
    }
  }
  if (identical(engine, "ctsem") && !is.null(data) && is_ild(data)) {
    st_dim <- tryCatch(as.numeric(bundle$fit$state_dimension)[1L], error = function(e) NA_real_)
    n <- nrow(data)
    min_n <- if (is.finite(st_dim) && st_dim > 1) 40 else 25
    if (is.finite(n) && n < min_n) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_CTSEM_SHORT_SERIES_FOR_COMPLEX_DYNAMICS",
        triggered = TRUE,
        message = sprintf(
          "n = %d observations; recommend at least %d for this ctsem complexity.",
          n, min_n
        ),
        recommendation = NULL
      )
    }
  }
  guardrail_finalize_rows(rows)
}

#' @keywords internal
#' @noRd
guardrail_dropout_late_heuristic <- function(data, outcome) {
  if (!outcome %in% names(data)) return(FALSE)
  ord <- order(data[[".ild_seq"]], seq_len(nrow(data)))
  miss <- is.na(data[[outcome]])
  miss <- miss[ord]
  n <- length(miss)
  if (n < 20L) return(FALSE)
  mid <- floor(n / 2)
  r1 <- mean(miss[seq_len(mid)])
  r2 <- mean(miss[(mid + 1L):n])
  r2 > pmax(1.5 * r1, r1 + 0.05, na.rm = TRUE)
}

#' Evaluate fit-level guardrails (frequentist + Bayesian)
#' @keywords internal
#' @noRd
evaluate_guardrails_fit <- function(object, fit_diag, engine = c("lmer", "lme", "brms", "ctsem")) {
  engine <- match.arg(engine)
  fd <- fit_bundle_flat_for_guardrails(fit_diag, engine = engine)
  rows <- list()
  if (engine %in% c("lmer", "lme")) {
    sing <- fd$singular
    if (!is.na(sing) && isTRUE(sing)) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_SINGULAR_RANDOM_EFFECTS",
        triggered = TRUE,
        message = NULL,
        recommendation = NULL
      )
    }
  }
  if (engine %in% c("lmer", "lme", "brms")) {
    het <- fit_diag$heterogeneity
    if (!is.null(het) && isTRUE(het$available) && !is.null(het$object)) {
      sig <- tryCatch(stats::sigma(object), error = function(e) NA_real_)
      sm <- het$object$summary
      # isTRUE guards nrow/sig comparisons that can be NA; skip heuristics when sigma is
      # numerically degenerate (e.g. failed lmer) to avoid false guardrails / if(NA).
      sig_ok <- is.finite(sig) && isTRUE(sig > 1e-8)
      if (!is.null(sm) && isTRUE(nrow(sm) > 0L) && sig_ok) {
        hit_var <- FALSE
        for (k in seq_len(nrow(sm))) {
          termk <- as.character(sm$term[k])
          if (termk %in% c("(Intercept)", "Intercept")) {
            next
          }
          vcs <- sm$varcorr_sdcor[k]
          if (is.finite(vcs) && vcs < 0.01 * sig) {
            hit_var <- TRUE
            break
          }
        }
        if (hit_var) {
          rows[[length(rows) + 1L]] <- list(
            rule_id = "GR_RE_SLOPE_VARIANCE_VERSUS_RESIDUAL_LOW",
            triggered = TRUE,
            message = NULL,
            recommendation = NULL
          )
        }
        sdt_max <- suppressWarnings(max(sm$sd_total, na.rm = TRUE))
        if (is.finite(sdt_max) && sdt_max < 0.05 * sig) {
          rows[[length(rows) + 1L]] <- list(
            rule_id = "GR_PERSON_SPECIFIC_SLOPES_EMPIRICALLY_TIGHT",
            triggered = TRUE,
            message = NULL,
            recommendation = NULL
          )
        }
      }
    }
  }
  if (engine == "brms") {
    mx <- fd$max_rhat
    if (!is.na(mx) && mx > 1.05) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_POOR_POSTERIOR_MIXING",
        triggered = TRUE,
        message = sprintf("Max R-hat = %.3f (threshold 1.05).", mx),
        recommendation = NULL
      )
    }
    ct <- fd$convergence_table
    if (!is.null(ct) && nrow(ct) > 0L && "ess_bulk" %in% names(ct)) {
      mness <- suppressWarnings(min(ct$ess_bulk, na.rm = TRUE))
      if (is.finite(mness) && mness < 100) {
        rows[[length(rows) + 1L]] <- list(
          rule_id = "GR_LOW_ESS",
          triggered = TRUE,
          message = sprintf("Minimum bulk ESS = %.0f (threshold 100).", mness),
          recommendation = NULL
        )
      }
    }
  }
  if (engine == "ctsem") {
    conv <- tryCatch(fit_diag$convergence$converged, error = function(e) NA)
    if (!is.na(conv) && !isTRUE(conv)) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_CTSEM_NONCONVERGENCE",
        triggered = TRUE,
        message = NULL,
        recommendation = NULL
      )
    }
    # Heuristic instability signal: very large absolute drift entries.
    drift_max <- tryCatch(as.numeric(fit_diag$drift_abs_max)[1L], error = function(e) NA_real_)
    if (is.finite(drift_max) && drift_max > 5) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_CTSEM_UNSTABLE_DYNAMICS",
        triggered = TRUE,
        message = sprintf("Maximum absolute drift estimate = %.2f (heuristic threshold > 5).", drift_max),
        recommendation = NULL
      )
    }
  }
  guardrail_finalize_rows(rows)
}

#' @keywords internal
#' @noRd
guardrails_bind <- function(...) {
  parts <- list(...)
  nonempty <- parts[vapply(parts, function(z) nrow(z) > 0L, logical(1))]
  if (length(nonempty) == 0L) {
    return(guardrails_empty_tibble())
  }
  dplyr::bind_rows(nonempty)
}
