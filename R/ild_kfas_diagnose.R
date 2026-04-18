# ild_diagnose() for ild_fit_kfas — diagnostics-centered bundle (KFAS)

#' @rdname ild_diagnose
#' @method ild_diagnose ild_fit_kfas
#' @export
ild_diagnose.ild_fit_kfas <- function(object,
                                      data = NULL,
                                      type = NULL,
                                      by_id = NULL,
                                      ...) {
  rlang::check_installed("KFAS", reason = "to diagnose ild_kfas fits")
  data <- if (is.null(data)) attr(object, "ild_data", exact = TRUE) else data
  if (is.null(data)) {
    stop("ild_diagnose() for ild_fit_kfas requires `data` or attr(fit, \"ild_data\").", call. = FALSE)
  }
  validate_ild(data)
  preds <- NULL
  rn <- attr(object, "ild_response", exact = TRUE)
  outcome_vars <- unique(c(stats::na.omit(c(rn, preds))))
  outcome_vars <- outcome_vars[outcome_vars %in% names(data)]

  kfs <- object$kfs
  y <- data[[rn]]
  y <- if (is.numeric(y)) y else suppressWarnings(as.numeric(y))
  res_diag <- ild_kfas_residual_diagnostics(kfs, y)

  innov <- tryCatch(as.numeric(stats::rstandard(kfs)), error = function(e) NULL)
  ex <- ild_kfas_extract_augment(kfs, y, TRUE)
  mae <- mean(abs(ex$resid), na.rm = TRUE)
  rmse <- sqrt(mean(ex$resid^2, na.rm = TRUE))
  bias <- mean(ex$resid, na.rm = TRUE)

  opt <- object$fit_ssm$optim.out
  loglik <- tryCatch(as.numeric(kfs$logLik), error = function(e) NA_real_)
  diffuse_note <- NULL
  ddiff <- tryCatch(as.integer(kfs$d), error = function(e) NA_integer_)
  if (is.finite(ddiff) && ddiff > 0L) {
    diffuse_note <- sprintf("Diffuse initialization phase ended after %d time index(es); see KFS() output.", ddiff)
  }

  spec <- object$spec
  map <- object$mapping
  pre <- object$preprocessing

  has_wpbp <- any(grepl("_wp$|_bp$", names(data)))

  fd <- list(
    engine = "KFAS",
    state_spec = spec$state_spec,
    observation_family = spec$observation_family,
    convergence = list(
      converged = tryCatch(isTRUE(opt$convergence == 0L), error = function(e) NA),
      note = "ML via KFAS::fitSSM; see fit$fit_ssm$optim.out."
    ),
    rank = list(note = "Not applicable (SSModel)."),
    optimizer = list(
      method = tryCatch(opt$method, error = function(e) NA_character_),
      convergence_code = tryCatch(opt$convergence, error = function(e) NA_integer_),
      value = tryCatch(opt$value, error = function(e) NA_real_)
    ),
    log_likelihood = loglik,
    diffuse_initialization = list(
      d = ddiff,
      note = diffuse_note
    ),
    state_dimension = map$state_dimension,
    observation_dimension = map$observation_dimension,
    residual_correlation = list(
      modeled = TRUE,
      note = paste(
        "Temporal dependence is modeled through the latent state (",
        spec$state_spec,
        "); standardized one-step-ahead innovations are primary residual objects.",
        sep = ""
      )
    ),
    random_effects_structure = list(
      note = "Latent states in fit$kfs$alphahat; see ild_tidy_states()."
    )
  )

  pred <- list(
    obs_metrics = list(
      engine = "KFAS",
      n = nrow(data),
      mae = mae,
      rmse = rmse,
      mean_residual = bias,
      mean_abs_innovation = if (!is.null(innov)) mean(abs(innov), na.rm = TRUE) else NA_real_
    ),
    simulation_checks = list(ppc = NULL),
    one_step_ahead = list(
      standardized_innovations = res_diag$standardized_innovations_summary
    )
  )

  residual_block <- list(
    engine = "KFAS",
    standardized_prediction_errors = res_diag$standardized_innovations_summary,
    acf = res_diag$acf_innovations,
    qq = res_diag$qq,
    outliers = res_diag$outlier_flags,
    ljung_box = res_diag$ljung_box,
    legacy_ild_diagnostics = NULL
  )

  warn_rows <- list()
  if (!is.null(opt) && !is.null(opt$message) && nzchar(paste(opt$message, collapse = ""))) {
    warn_rows[[length(warn_rows) + 1L]] <- list(
      source = "KFAS",
      level = "note",
      message = paste(as.character(opt$message), collapse = " "),
      code = "kfas_optim_message"
    )
  }
  qv <- ild_kfas_qh_scalar(object$kfas_model$Q)
  hv <- ild_kfas_qh_scalar(object$kfas_model$H)
  if (is.finite(qv) && qv < 1e-8) {
    warn_rows[[length(warn_rows) + 1L]] <- list(
      source = "tidyILD",
      level = "warning",
      message = "Estimated state variance Q is effectively zero (boundary).",
      code = "kfas_degenerate_Q"
    )
  }
  if (is.finite(hv) && hv < 1e-8) {
    warn_rows[[length(warn_rows) + 1L]] <- list(
      source = "tidyILD",
      level = "warning",
      message = "Estimated observation variance H is effectively zero (boundary).",
      code = "kfas_degenerate_H"
    )
  }

  warn_df <- if (length(warn_rows) > 0L) {
    tibble::tibble(
      source = vapply(warn_rows, function(z) z$source, character(1L)),
      level = vapply(warn_rows, function(z) z$level, character(1L)),
      message = vapply(warn_rows, function(z) z$message, character(1L)),
      code = vapply(warn_rows, function(z) z$code, character(1L))
    )
  } else {
    tibble::tibble()
  }

  sm_data <- tryCatch(ild_summary(data), error = function(e) NULL)
  time_span <- if (!is.null(sm_data)) {
    list(
      min = sm_data$summary$time_min[1],
      max = sm_data$summary$time_max[1],
      median_dt_sec = sm_data$summary$median_dt_sec[1],
      iqr_dt_sec = sm_data$summary$iqr_dt_sec[1]
    )
  } else {
    NULL
  }

  data_sec <- fill_diagnostics_data(data, outcome_vars = outcome_vars)
  data_sec$intervals_summary <- time_span
  data_sec$n_ids <- dplyr::n_distinct(data[[".ild_id"]])
  data_sec$kfas_preprocessing <- pre

  design_sec <- fill_diagnostics_design(data, vars = NULL)
  design_sec$irregular_spacing_class <- tryCatch(
    ild_spacing_class(data),
    error = function(e) NA_character_
  )
  design_sec$predictors_wp_bp_decomposed <- has_wpbp

  bundle <- ild_diagnostics_bundle(
    meta = list(
      engine = "KFAS",
      model_type = spec$state_spec,
      state_spec = spec$state_spec,
      observation_family = spec$observation_family,
      smoother = isTRUE(map$smoother),
      filter_smoother_settings = list(
        kfs_smoothing = isTRUE(map$smoother),
        kfs_filtering = "state"
      ),
      pool_mode = map$pool_mode,
      fit_context = spec$fit_context,
      n_obs = nrow(data),
      n_id = dplyr::n_distinct(data[[".ild_id"]]),
      time_units = spec$time_units
    ),
    data = data_sec,
    design = design_sec,
    fit = fd,
    residual = residual_block,
    predictive = pred,
    missingness = fill_diagnostics_missingness_section(data, vars = outcome_vars),
    causal = fill_diagnostics_causal(data, causal_detail = FALSE),
    warnings = warn_df,
    guardrails = guardrails_empty_tibble(),
    summary_text = character()
  )

  bundle$guardrails <- evaluate_guardrails_kfas(object, data, bundle)
  bundle$summary_text <- build_diagnostics_bundle_summary(bundle)
  bundle <- enrich_bundle_semantic_sections(bundle)
  attr(bundle, "ild_fit") <- object
  attr(bundle, "ild_data") <- data
  bundle
}
