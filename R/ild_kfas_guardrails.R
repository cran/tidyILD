# KFAS-specific guardrails for ild_diagnose.ild_fit_kfas

#' Evaluate KFAS methodological guardrails
#' @keywords internal
#' @noRd
evaluate_guardrails_kfas <- function(object, data, bundle = NULL) {
  rows <- list()
  if (!inherits(object, "ild_fit_kfas")) {
    return(guardrail_finalize_rows(rows))
  }

  spec <- object$spec
  n <- nrow(data)
  sm <- tryCatch(ild_summary(data), error = function(e) NULL)
  sc <- tryCatch(ild_spacing_class(data), error = function(e) NA_character_)

  # GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME
  if (!is.null(sm) && !is.na(sc) && sc == "irregular-ish") {
    med <- sm$summary$median_dt_sec[1]
    iqr <- sm$summary$iqr_dt_sec[1]
    cv <- if (is.finite(med) && med > 0 && is.finite(iqr)) iqr / med else NA_real_
    if (is.finite(cv) && cv > 0.75) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_KFAS_HIGH_IRREGULARITY_FOR_DISCRETE_TIME",
        triggered = TRUE,
        message = sprintf(
          "IQR/median interval ratio = %.2f on discrete-time indexing; KFAS treats steps as uniform.",
          cv
        ),
        recommendation = NULL
      )
    }
  }

  # GR_KFAS_SHORT_SERIES_FOR_STATE_SPACE — local level needs sufficient n
  state_spec <- spec$state_spec
  min_n <- if (identical(state_spec, "local_level")) 15L else 25L
  if (n < min_n) {
    rows[[length(rows) + 1L]] <- list(
      rule_id = "GR_KFAS_SHORT_SERIES_FOR_STATE_SPACE",
      triggered = TRUE,
      message = sprintf("n = %d time points; recommend at least ~%d for stable %s estimation.", n, min_n, state_spec),
      recommendation = NULL
    )
  }

  # GR_KFAS_STATE_DIMENSION_HIGH_FOR_N
  m_state <- tryCatch(ncol(object$kfs$alphahat), error = function(e) NA_integer_)
  if (is.finite(m_state) && m_state >= 2L && n < 20L * m_state) {
    rows[[length(rows) + 1L]] <- list(
      rule_id = "GR_KFAS_STATE_DIMENSION_HIGH_FOR_N",
      triggered = TRUE,
      message = sprintf(
        "Smoothed state dimension = %d with n = %d; complexity may exceed information in the series.",
        m_state, n
      ),
      recommendation = NULL
    )
  }

  # GR_KFAS_DEGENERATE_VARIANCE_ESTIMATE
  fm <- object$kfas_model
  qv <- ild_kfas_qh_scalar(fm$Q)
  hv <- ild_kfas_qh_scalar(fm$H)
  eps <- 1e-8
  if ((is.finite(qv) && qv < eps) || (is.finite(hv) && hv < eps)) {
    rows[[length(rows) + 1L]] <- list(
      rule_id = "GR_KFAS_DEGENERATE_VARIANCE_ESTIMATE",
      triggered = TRUE,
      message = sprintf(
        "Estimated variances Q = %g, H = %g may be on the boundary.",
        qv, hv
      ),
      recommendation = NULL
    )
  }

  # GR_KFAS_NONCONVERGENCE
  opt <- object$fit_ssm$optim.out
  if (!is.null(opt)) {
    conv <- opt$convergence
    if (!is.null(conv) && !isTRUE(conv == 0L)) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_KFAS_NONCONVERGENCE",
        triggered = TRUE,
        message = sprintf("Optimizer convergence code %s (see fit$fit_ssm$optim.out).", conv),
        recommendation = NULL
      )
    }
  }

  # GR_KFAS_MANY_MISSING_OUTCOME_SEGMENTS
  pre <- object$preprocessing
  if (!is.null(pre) && is.list(pre)) {
    nseg <- pre$n_na_segments
    pct <- pre$pct_outcome_na
    if (is.finite(nseg) && nseg >= 3L && is.finite(pct) && pct > 0.1) {
      rows[[length(rows) + 1L]] <- list(
        rule_id = "GR_KFAS_MANY_MISSING_OUTCOME_SEGMENTS",
        triggered = TRUE,
        message = sprintf(
          "%d separate NA segments in the outcome (%.0f%% NA before omitting rows).",
          nseg, 100 * pct
        ),
        recommendation = NULL
      )
    }
  }

  # GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY
  fc <- spec$fit_context
  if (identical(fc, "independent_series_per_id")) {
    rows[[length(rows) + 1L]] <- list(
      rule_id = "GR_KFAS_UNMODELED_BETWEEN_PERSON_HETEROGENEITY",
      triggered = TRUE,
      message = NULL,
      recommendation = NULL
    )
  }

  guardrail_finalize_rows(rows)
}
