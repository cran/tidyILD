# Methods narrative from provenance
# Uses ild_provenance(), ild_flatten_provenance() from ild_provenance.R
# Step order is preserved exactly (data steps then analysis steps).

# Format a "created" vector for methods text (internal)
.format_created <- function(o) {
  if (is.null(o) || length(o) == 0) return("")
  cre <- o$created
  if (is.null(cre)) return("")
  if (length(cre) == 1) return(paste0(", creating ", cre, "."))
  paste0(", creating ", paste(cre, collapse = ", "), ".")
}

#' Turn one step record into a methods sentence (internal)
#' @param s Step record (list with step, args, outputs).
#' @param context Optional. The object x passed to ild_methods(); used to read ild_robust_se when step is ild_lme.
#' @param robust_se Optional. Passed from ild_methods(robust_se = ); used when step is ild_lme to mention cluster-robust SEs.
#' @noRd
ild_step_to_sentence <- function(s, context = NULL, robust_se = NULL) {
  step <- s$step
  a <- s$args
  o <- s$outputs
  if (step == "ild_prepare") {
    if (is.numeric(a$gap_threshold) && is.finite(a$gap_threshold)) {
      gt_phrase <- paste0("a gap threshold of ", a$gap_threshold, " units")
    } else {
      gt_phrase <- "no gap threshold"
    }
    out <- paste0(
      "Data were prepared using ild_prepare() with participant ID ", .def(a$id, "id"),
      ", time variable ", .def(a$time, "time"),
      ", and ", gt_phrase
    )
    if (!is.null(o$spacing_class) && length(o$spacing_class) == 1 && nchar(o$spacing_class) > 0) {
      out <- paste0(out, " (spacing class: ", o$spacing_class, ")")
    }
    if (!is.null(o$n_id) && !is.null(o$n_obs)) {
      out <- paste0(out, " (N = ", o$n_id, " persons, n = ", o$n_obs, " observations)")
    }
    return(paste0(out, "."))
  }
  if (step == "ild_center") {
    vars <- a$vars
    if (length(vars) > 1) vars <- paste(vars, collapse = ", ")
    type <- .def(a$type, "person_mean")
    out <- paste0(
      "Predictor(s) ", vars, " were ",
      if (type == "grand_mean") "grand-mean" else "person-mean",
      " centered using ild_center()"
    )
    out <- paste0(out, .format_created(o))
    if (!endsWith(out, ".")) out <- paste0(out, ".")
    return(out)
  }
  if (step == "ild_lag") {
    vars <- a$vars
    if (length(vars) > 1) vars <- paste(vars, collapse = ", ")
    n <- .def(a$n, 1)
    mode <- .def(a$mode, "index")
    max_gap <- a$max_gap
    if (is.null(max_gap) || (is.numeric(max_gap) && !is.finite(max_gap))) max_gap <- "none"
    out <- paste0(
      "A ", mode, " lag of ", vars, " was computed using ild_lag() with lag ", n,
      " and max gap ", max_gap
    )
    if (identical(mode, "time_window") && (!is.null(a$window) || !is.null(a$resolution))) {
      out <- paste0(out, ", time window ", .def(a$window, "as specified"),
        if (!is.null(a$resolution)) paste0(" and resolution ", a$resolution) else "")
    }
    out <- paste0(out, .format_created(o))
    if (!endsWith(out, ".")) out <- paste0(out, ".")
    return(out)
  }
  if (step == "ild_align") {
    out <- paste0(
      "Variable ", .def(a$value_var, "value"), " was aligned from secondary data using ild_align() with window ", .def(a$window, "window")
    )
    out <- paste0(out, .format_created(o))
    if (!endsWith(out, ".")) out <- paste0(out, ".")
    return(out)
  }
  if (step == "ild_ipw_weights") {
    stab <- if (isTRUE(a$stabilize)) "stabilized" else "unstabilized"
    out <- paste0("Inverse probability weights (", stab, ") were added using ild_ipw_weights()")
    if (!is.null(a$trim) && length(a$trim) >= 2) {
      out <- paste0(out, " with trim quantiles ", paste(a$trim, collapse = " and "))
    }
    out <- paste0(out, .format_created(o))
    if (!endsWith(out, ".")) out <- paste0(out, ".")
    return(out)
  }
  if (step == "ild_lme") {
    engine <- .def(a$method, "lmer")
    out <- paste0(
      "A mixed-effects model was fit using ild_lme() (", engine, ") with formula ", .def(a$formula, "as specified")
    )
    if (isTRUE(a$ar1) && !is.null(a$correlation_class) && !is.na(a$correlation_class) && nchar(a$correlation_class) > 0) {
      out <- paste0(out, " and ", a$correlation_class, " correlation structure for residuals")
    } else {
      out <- paste0(out, " with AR1 disabled")
    }
    if (!is.null(o$n_obs) && !is.null(o$n_id)) {
      out <- paste0(out, " (n = ", o$n_obs, " observations, N = ", o$n_id, " persons)")
    }
    out <- paste0(out, ".")
    rs <- robust_se
    if (is.null(rs) && !is.null(context)) {
      rs <- attr(context, "ild_robust_se", exact = TRUE)
      if (is.null(rs) && is.list(context) && !is.null(context$fit)) {
        rs <- attr(context$fit, "ild_robust_se", exact = TRUE)
      }
    }
    if (length(rs) == 1 && is.character(rs) && nchar(rs) > 0) {
      out <- paste0(out, " Fixed effects were reported with cluster-robust standard errors (", rs, ").")
    }
    return(out)
  }
  if (step == "ild_brms") {
    pr <- a$prior_summary_text
    if (is.null(pr) || !nzchar(as.character(pr)[1])) pr <- "see prior_summary(fit)"
    pr_short <- substr(gsub("\n", " ", as.character(pr)), 1L, 500L)
    if (nchar(as.character(pr)) > 500) pr_short <- paste0(pr_short, " [...]")
    out <- paste0(
      "A Bayesian mixed-effects model was fit using ild_brms() with formula ", .def(a$formula, "as specified"),
      ". Sampling used ", .def(a$chains, "?"), " chains, ", .def(a$iter, "?"), " iterations per chain (warmup ",
      .def(a$warmup, "?"), "). ",
      "Sampler: adapt_delta = ", .def(a$adapt_delta, "default"),
      ", max_treedepth = ", .def(a$max_treedepth, "default"),
      if (!is.null(a$seed) && !is.na(a$seed)) paste0(", seed = ", a$seed) else "",
      ". Priors (brms::prior_summary): ", pr_short
    )
    if (!is.null(o$n_obs) && !is.null(o$n_id)) {
      out <- paste0(out, " (n = ", o$n_obs, ", N = ", o$n_id, " persons)")
    }
    out <- paste0(out, ".")
    if (!is.null(o$max_rhat) && is.finite(o$max_rhat)) {
      out <- paste0(out, " Max R-hat: ", round(o$max_rhat, 3), ".")
    }
    return(out)
  }
  if (step == "ild_kfas") {
    st <- .def(a$state_spec, "local_level")
    fam <- .def(a$observation_family, "gaussian")
    tu <- .def(a$time_units, "time units")
    fc <- .def(a$fit_context, "single_series")
    sm <- if (isTRUE(a$smoother)) {
      "smoothed and filtered state estimates (KFS smoothing)"
    } else {
      "filtered state estimates only (smoothing off)"
    }
    struct <- if (identical(st, "local_level")) {
      "local level (random walk plus observation noise)"
    } else {
      st
    }
    dots_note <- ""
    if (!is.null(a$fit_ssm_dots) && length(a$fit_ssm_dots) > 0L) {
      dn <- paste(names(a$fit_ssm_dots), a$fit_ssm_dots, sep = " = ", collapse = "; ")
      if (nzchar(dn)) dots_note <- paste0(" Additional fitSSM controls: ", dn, ".")
    }
    fh <- a$forecast_horizon
    fh_note <- if (!is.null(fh) && is.finite(fh) && fh >= 1L) {
      paste0(" Forecast horizon was set to ", fh, " step(s).")
    } else {
      ""
    }
    out <- paste0(
      "A Gaussian state-space model was fit using ild_kfas() with outcome ", .def(a$outcome, "outcome"),
      ", latent structure ", struct, " (", fam, " observations), and time indexing in ", tu, ". ",
      "KFS used ", sm, ". ",
      "Fit context: ", fc, " (not pooled across persons unless you explicitly stacked multiple series).",
      if (isTRUE(a$irregular_time)) {
        " Irregular observation spacing was acknowledged (irregular_time = TRUE)."
      } else {
        " Equal spacing was assumed for the time index after preparation."
      },
      fh_note,
      dots_note
    )
    if (!is.null(o$n_obs)) {
      out <- paste0(out, " (n = ", o$n_obs, " time points")
      if (!is.null(o$n_id)) out <- paste0(out, ", N = ", o$n_id, " person(s)")
      out <- paste0(out, ").")
    } else {
      out <- paste0(out, ".")
    }
    if (!is.null(o$logLik) && is.finite(o$logLik)) {
      out <- paste0(out, " Log-likelihood: ", round(o$logLik, 2), ".")
    }
    return(out)
  }
  if (step == "ild_ctsem") {
    out <- paste0(
      "A continuous-time latent-dynamics model was fit using ild_ctsem() with outcome ",
      .def(a$outcome, "outcome"),
      ", fit type ", .def(a$model_type, "ctsem default"),
      ", id column ", .def(a$id_col, ".ild_id"),
      ", and time column ", .def(a$time_col, ".ild_time_num"),
      "."
    )
    if (!is.null(o$n_obs) && !is.null(o$n_id)) {
      out <- paste0(out, " (n = ", o$n_obs, ", N = ", o$n_id, " persons).")
    }
    if (!is.null(o$converged) && !is.na(o$converged)) {
      out <- paste0(out, " Converged: ", o$converged, ".")
    }
    return(out)
  }
  if (step == "ild_diagnostics") {
    types <- a$type
    if (is.null(types) || length(types) == 0) {
      return("Residual diagnostics were examined.")
    }
    labels <- character(0)
    if ("residual_acf" %in% types) labels <- c(labels, "autocorrelation")
    if ("residual_time" %in% types) labels <- c(labels, "residuals vs time and fitted")
    if ("qq" %in% types) labels <- c(labels, "Q-Q")
    if (length(labels) == 0) labels <- "requested diagnostics"
    return(paste0("Residual diagnostics were examined using ", paste(labels, collapse = " and "), " diagnostics."))
  }
  if (step == "ild_tvem") {
    return(paste0(
      "A time-varying effects model was fit using ild_tvem() for outcome ", .def(a$outcome, "outcome"),
      " and predictor ", .def(a$predictor, "predictor"), "."
    ))
  }
  if (step == "ild_power") {
    return(paste0(
      "Simulation-based power analysis was run with n_sim=", .def(a$n_sim, "?"), ", effect_size=", .def(a$effect_size, "?"), ", test_term=", .def(a$test_term, "?"), "."
    ))
  }
  if (step == "ild_missing_model") {
    preds <- a$predictors
    if (length(preds) > 1) preds <- paste(preds, collapse = ", ")
    return(paste0(
      "A missingness model was fit for outcome ", .def(a$outcome, "outcome"),
      " and predictors ", preds, "."
    ))
  }
  if (step == "ild_crosslag") {
    out <- paste0(
      "A cross-lag model was fit using ild_crosslag() with outcome ", .def(a$outcome, "outcome"),
      ", predictor ", .def(a$predictor, "predictor"), ", lag ", .def(a$lag, 1)
    )
    if (isTRUE(a$ar1)) out <- paste0(out, " and AR1 correlation") else out <- paste0(out, " and AR1 disabled")
    return(paste0(out, "."))
  }
  if (step == "ild_ipw_refit") {
    wcol <- .def(a$weights, ".ipw")
    return(paste0(
      "Weighted estimation was used: the model was refit with inverse-probability weights using ild_ipw_refit() (weight column: ", wcol, ")."
    ))
  }
  paste0("Step ", step, " was applied.")
}

# Default value helper (internal)
.def <- function(x, y) if (is.null(x)) y else x

#' Generate methods-style narrative from provenance
#'
#' Takes an ILD data object, a model fit, or a diagnostics object and produces
#' a concise methods-style paragraph based on the recorded provenance (data
#' preparation, centering, lagging, modeling, etc.).
#'
#' @param x An ILD object (see [is_ild()]), a model from [ild_lme()] or [ild_tvem()],
#'   a diagnostics object from [ild_diagnostics()], or another object with
#'   [ild_provenance()] (e.g. [ild_power()] result, [ild_missing_model()] result).
#' @param robust_se Optional. If you reported fixed effects with cluster-robust SEs
#'   via [tidy_ild_model()] with \code{se = "robust"}, pass the type here (e.g. \code{"CR2"})
#'   so the methods text can mention it.
#' @param bundle Optional. An object from [ild_diagnose()] (class
#'   \code{\link{ild_diagnostics_bundle}}). When provided and
#'   \code{nrow(bundle$guardrails) > 0}, a short methodological-cautions sentence
#'   is appended after the provenance paragraph.
#' @param ... Unused.
#' @return A single character string (one or more sentences) suitable for a
#'   methods section. Use \code{cat()} or \code{print()} to display.
#' @details
#' After fitting, call \code{diag <- ild_diagnose(fit)} and pass
#' \code{bundle = diag} to surface triggered guardrails in the methods narrative
#' without duplicating [ild_report()]'s structured \code{diagnostics_summary}.
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' x <- ild_center(x, y)
#' ild_methods(x)
ild_methods <- function(x, robust_se = NULL, bundle = NULL, ...) {
  prov <- ild_provenance(x)
  if (is.null(prov)) {
    base <- "No provenance recorded for this object."
  } else {
    steps <- ild_flatten_provenance(prov)
    if (length(steps) == 0) {
      base <- "Provenance has no steps recorded."
    } else {
      sentences <- vapply(steps, function(s) ild_step_to_sentence(s, context = x, robust_se = robust_se), character(1L))
      base <- paste(sentences, collapse = " ")
    }
  }
  if (inherits(bundle, "ild_diagnostics_bundle") && nrow(bundle$guardrails) > 0L) {
    gs <- ild_guardrails_summary(bundle)
    if (nzchar(gs$narrative)) {
      base <- paste0(
        base,
        " Methodological cautions (tidyILD guardrails): ",
        gs$narrative
      )
    }
  }
  base
}

#' Assemble a light report from a model fit
#'
#' Builds a structured list with methods narrative (from [ild_methods()]),
#' fixed-effects table (from [tidy_ild_model()]), a short diagnostics summary,
#' and the raw provenance. Optionally exports provenance to a file.
#'
#' @param fit A fitted model from [ild_lme()] (or a list with a \code{fit} component, e.g. from [ild_crosslag()]).
#' @param export_provenance_path Optional. If provided, [ild_export_provenance()] is called to write provenance to this path; the path is included in the returned list.
#' @param robust_se Optional. Passed to [ild_methods()] when building the methods text (e.g. \code{"CR2"} if you used [tidy_ild_model()] with \code{se = "robust"}).
#' @param ... Unused.
#' @return A list with a stable schema: \code{meta} (list with \code{n_obs}, \code{n_id}, \code{engine} when available), \code{methods}, \code{model_table}, \code{diagnostics_summary} (includes \code{guardrails_narrative} and \code{guardrails} when [ild_diagnose()] succeeds), \code{provenance}, \code{provenance_export_path} (character or NULL). When guardrails are triggered, \code{methods_with_guardrails} repeats the methods paragraph with guardrails appended (same pattern as \code{\link{ild_methods}(fit, bundle = d)}).
#' @export
#' @examples
#' set.seed(1)
#' x <- ild_prepare(ild_simulate(n_id = 5, n_obs_per = 6, seed = 1), id = "id", time = "time")
#' fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
#' r <- ild_report(fit)
#' r$methods
#' r$model_table
ild_report <- function(fit, export_provenance_path = NULL, robust_se = NULL, ...) {
  if (is.list(fit) && !is.null(fit$fit)) fit <- fit$fit
  meta <- list(n_obs = NULL, n_id = NULL, engine = NULL)
  ild_data <- attr(fit, "ild_data", exact = TRUE)
  if (!is.null(ild_data) && nrow(ild_data) > 0) {
    meta$n_obs <- nrow(ild_data)
    id_col <- ild_meta(ild_data)$ild_id
    if (!is.null(id_col) && id_col %in% names(ild_data)) {
      meta$n_id <- length(unique(ild_data[[id_col]]))
    }
  }
  meta$engine <- if (inherits(fit, "ild_fit_kfas")) {
    "KFAS"
  } else if (inherits(fit, "ild_fit_ctsem")) {
    "ctsem"
  } else if (inherits(fit, "lme")) {
    "lme"
  } else if (inherits(fit, "lmerMod")) {
    "lmer"
  } else if (inherits(fit, "brmsfit")) {
    "brms"
  } else {
    NULL
  }
  diag_bundle <- tryCatch(
    ild_diagnose(
      fit,
      type = if (inherits(fit, "brmsfit")) {
        "all"
      } else if (inherits(fit, "ild_fit_kfas")) {
        NULL
      } else if (inherits(fit, "ild_fit_ctsem")) {
        NULL
      } else {
        c("residual_acf", "qq")
      }
    ),
    error = function(e) NULL
  )
  methods_text <- ild_methods(fit, robust_se = robust_se)
  methods_with_guardrails <- if (!is.null(diag_bundle)) {
    ild_methods(fit, robust_se = robust_se, bundle = diag_bundle)
  } else {
    NULL
  }
  model_table <- tryCatch(
    if (inherits(fit, "brmsfit") || inherits(fit, "ild_fit_kfas")) {
      ild_tidy(fit)
    } else {
      tidy_ild_model(fit, object = FALSE)
    },
    error = function(e) NULL
  )
  diagnostics_summary <- if (is.null(diag_bundle)) {
    list(meta = NULL, summary_text = "Diagnostics could not be computed.")
  } else {
    gs <- ild_guardrails_summary(diag_bundle$guardrails)
    out <- list(
      meta = diag_bundle$meta,
      summary_text = paste(diag_bundle$summary_text, collapse = " "),
      guardrails_narrative = gs$narrative,
      guardrails = list(
        n = gs$n,
        max_severity = gs$max_severity,
        rule_ids = gs$rule_ids
      )
    )
    if (gs$n == 0L) {
      out$guardrails_narrative <- ""
      out$guardrails <- list(n = 0L, max_severity = NA_character_, rule_ids = character(0))
    }
    out
  }
  provenance <- ild_provenance(fit)
  provenance_export_path <- NULL
  if (!is.null(export_provenance_path) && length(export_provenance_path) == 1L && is.character(export_provenance_path) && nchar(export_provenance_path) > 0 && !is.null(provenance)) {
    ild_export_provenance(fit, export_provenance_path)
    provenance_export_path <- export_provenance_path
  }
  out <- list(
    meta = meta,
    methods = methods_text,
    model_table = model_table,
    diagnostics_summary = diagnostics_summary,
    provenance = provenance,
    provenance_export_path = provenance_export_path
  )
  if (!is.null(methods_with_guardrails) && !identical(methods_with_guardrails, methods_text)) {
    out$methods_with_guardrails <- methods_with_guardrails
  }
  out
}
