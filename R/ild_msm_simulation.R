# MSM simulation and recovery harness (causal validation)

#' Simulate a simple longitudinal MSM scenario
#'
#' Generates ILD with time-varying treatment, confounding, and optional monotone
#' censoring/dropout under a known treatment effect (\code{true_ate}).
#'
#' @param n_id Number of persons.
#' @param n_obs_per Planned observations per person.
#' @param true_ate True additive treatment effect on outcome.
#' @param censoring If \code{TRUE}, apply monotone dropout and return observed rows only.
#' @param positivity_stress Multiplier for treatment/censoring logits
#'   (>1 worsens overlap; <1 improves overlap).
#' @param treatment_strength Base strength of treatment dependence on confounders/history.
#' @param censoring_strength Base strength of dropout dependence on stress/treatment.
#' @param seed Integer seed.
#' @return ILD object with columns \code{stress} and binary \code{trt}; attributes include
#'   \code{true_ate}.
#' @export
#' @examples
#' d <- ild_msm_simulate_scenario(n_id = 20, n_obs_per = 8, true_ate = 0.5, seed = 12)
#' ild_summary(d)$summary
ild_msm_simulate_scenario <- function(n_id = 60L,
                                      n_obs_per = 10L,
                                      true_ate = 0.5,
                                      censoring = TRUE,
                                      positivity_stress = 1,
                                      treatment_strength = 0.8,
                                      censoring_strength = 0.6,
                                      seed = 42L) {
  set.seed(as.integer(seed)[1L])
  d <- ild_simulate(n_id = n_id, n_obs_per = n_obs_per, seed = seed)
  d$stress <- as.numeric(scale(rnorm(nrow(d))))
  d <- ild_prepare(d, id = "id", time = "time")

  # Sequential treatment process A_t ~ stress_t + A_{t-1}
  ord <- order(d$.ild_id, d$.ild_seq, seq_len(nrow(d)))
  ido <- d$.ild_id[ord]
  stress_o <- d$stress[ord]
  trt_o <- rep(NA_integer_, nrow(d))
  i <- 1L
  while (i <= nrow(d)) {
    j <- i
    while (j < nrow(d) && ido[j + 1L] == ido[i]) j <- j + 1L
    prev <- 0
    for (k in i:j) {
      p <- stats::plogis(
        -0.2 + positivity_stress * (treatment_strength * stress_o[k] + 1.0 * prev)
      )
      trt_o[k] <- stats::rbinom(1L, 1L, p)
      prev <- trt_o[k]
    }
    i <- j + 1L
  }
  trt <- rep(NA_integer_, nrow(d))
  trt[ord] <- trt_o
  d$trt <- trt

  # Outcome with true causal treatment effect.
  d$y <- d$y + as.numeric(true_ate) * d$trt + 0.4 * d$stress

  if (isTRUE(censoring)) {
    keep <- rep(TRUE, nrow(d))
    for (pid in unique(d$.ild_id)) {
      idx <- which(d$.ild_id == pid)
      idx <- idx[order(d$.ild_seq[idx])]
      dropped <- FALSE
      for (k in seq_along(idx)) {
        ii <- idx[k]
        if (!dropped) {
          p_drop <- stats::plogis(
            -3.5 + positivity_stress * (censoring_strength * d$stress[ii] + 0.5 * d$trt[ii])
          )
          dr <- stats::rbinom(1L, 1L, p_drop)
          if (dr == 1L && k < length(idx)) {
            dropped <- TRUE
          }
        } else {
          keep[ii] <- FALSE
        }
      }
    }
    raw <- as.data.frame(d[keep, c("id", "time", "y", "stress", "trt"), drop = FALSE])
    d <- ild_prepare(raw, id = "id", time = "time")
  }

  attr(d, "true_ate") <- as.numeric(true_ate)
  d
}

#' MSM recovery simulation harness
#'
#' Repeats an MSM analysis over simulated datasets with known \code{true_ate} and
#' summarizes estimation bias, RMSE, CI coverage, and positivity stress summaries.
#'
#' @param n_sim Number of simulations.
#' @param n_id Number of persons per simulation.
#' @param n_obs_per Planned observations per person.
#' @param true_ate True additive treatment effect.
#' @param n_boot Bootstrap replicates when \code{inference = "bootstrap"}.
#' @param inference \code{"bootstrap"} (default), \code{"robust"}, or \code{"none"}.
#' @param seed Base seed.
#' @param censoring If \code{TRUE}, include monotone dropout in DGP and fit IPCW+joint pipeline.
#' @param scenario_grid Optional data frame/list of scenario settings. Supported columns:
#'   \code{scenario_id, n_id, n_obs_per, true_ate, censoring, positivity_stress,
#'   treatment_strength, censoring_strength, misspec_treatment_model}.
#' @return List with \code{summary}, \code{summary_by_scenario}, and \code{sim_results}.
#' @export
#' @examples
#' \dontrun{
#' out <- ild_msm_recovery(n_sim = 10, n_id = 40, n_obs_per = 10, true_ate = 0.5, n_boot = 50)
#' out$summary
#' }
ild_msm_recovery <- function(n_sim = 100L,
                             n_id = 80L,
                             n_obs_per = 10L,
                             true_ate = 0.5,
                             n_boot = 200L,
                             inference = c("bootstrap", "robust", "none"),
                             seed = 1001L,
                             censoring = TRUE,
                             scenario_grid = NULL) {
  inference <- match.arg(inference)
  n_sim <- as.integer(n_sim)[1L]
  if (!is.finite(n_sim) || n_sim < 1L) stop("n_sim must be >= 1.", call. = FALSE)
  scen <- .ild_msm_build_scenario_grid(
    scenario_grid = scenario_grid,
    n_id = n_id,
    n_obs_per = n_obs_per,
    true_ate = true_ate,
    censoring = censoring
  )
  rows <- vector("list", nrow(scen) * n_sim)
  at <- 1L
  for (j in seq_len(nrow(scen))) {
    sj <- scen[j, , drop = FALSE]
    for (i in seq_len(n_sim)) {
      s <- as.integer(seed) + i + (j - 1L) * 10000L
      dat <- ild_msm_simulate_scenario(
        n_id = sj$n_id[[1L]],
        n_obs_per = sj$n_obs_per[[1L]],
        true_ate = sj$true_ate[[1L]],
        censoring = sj$censoring[[1L]],
        positivity_stress = sj$positivity_stress[[1L]],
        treatment_strength = sj$treatment_strength[[1L]],
        censoring_strength = sj$censoring_strength[[1L]],
        seed = s
      )
      dat <- ild_center(dat, y)
      hs <- ild_msm_history_spec(vars = c("stress", "trt"), lags = 1L)
      dat <- ild_build_msm_history(dat, hs)
      est <- ild_msm_estimand(type = "ate", regime = "static", treatment = "trt")
      hist_formula <- if (isTRUE(sj$misspec_treatment_model[[1L]])) {
        ~ stress_lag1
      } else {
        ~ stress_lag1 + trt_lag1
      }

      fit_obj <- tryCatch(
        ild_msm_fit(
          estimand = est,
          data = dat,
          outcome_formula = y ~ y_bp + y_wp + stress + trt + (1 | id),
          history = hist_formula,
          treatment_engine = "sequential_msm",
          predictors_censor = if (isTRUE(sj$censoring[[1L]])) "stress" else NULL,
          weights_col = ".ipw",
          inference = inference,
          n_boot = n_boot,
          seed = s,
          warn_no_ar1 = FALSE,
          warn_uncentered = FALSE
        ),
        error = function(e) NULL
      )
      if (is.null(fit_obj)) {
        rows[[at]] <- tibble::tibble(
          scenario_id = sj$scenario_id[[1L]],
          true_ate = sj$true_ate[[1L]],
          sim = i,
          estimate = NA_real_,
          std_error = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_,
          covered = NA,
          ess = NA_real_,
          weight_ratio = NA_real_,
          propensity_extreme_rate = NA_real_,
          inference_status = "unsupported",
          inference_method = inference,
          converged = FALSE
        )
        at <- at + 1L
        next
      }

      coef_est <- as.numeric(lme4::fixef(fit_obj$fit)[["trt"]])
      rr <- fit_obj$inference$summary
      rr <- rr[rr$term == "trt", , drop = FALSE]
      se <- if (nrow(rr) == 1L) rr$std_error else NA_real_
      lo <- if (nrow(rr) == 1L) rr$conf_low else NA_real_
      hi <- if (nrow(rr) == 1L) rr$conf_high else NA_real_

      ess <- ild_ipw_ess(fit_obj$weights_data, weights_col = fit_obj$weights_col)
      wf <- fit_obj$weights_data[[fit_obj$weights_col]]
      wf <- wf[is.finite(wf) & wf > 0]
      wr <- if (length(wf) >= 2L) max(wf) / min(wf) else NA_real_
      overlap <- .ild_msm_propensity_overlap_summary(fit_obj$weights_data)

      rows[[at]] <- tibble::tibble(
        scenario_id = sj$scenario_id[[1L]],
        true_ate = sj$true_ate[[1L]],
        sim = i,
        estimate = coef_est,
        std_error = se,
        conf_low = lo,
        conf_high = hi,
        covered = is.finite(lo) && is.finite(hi) && lo <= sj$true_ate[[1L]] && sj$true_ate[[1L]] <= hi,
        ess = ess,
        weight_ratio = wr,
        propensity_extreme_rate = overlap$extreme_rate,
        inference_status = fit_obj$inference$status,
        inference_method = fit_obj$inference$method,
        converged = TRUE
      )
      at <- at + 1L
    }
  }

  sim_results <- dplyr::bind_rows(rows)
  summary_by_scenario <- .ild_msm_recovery_summary_by_scenario(sim_results)
  summary <- .ild_msm_recovery_summary_overall(summary_by_scenario, n_sim = n_sim)

  out <- list(
    summary = summary,
    summary_by_scenario = summary_by_scenario,
    sim_results = sim_results,
    scenarios = scen
  )
  class(out) <- c("ild_msm_recovery", "list")
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(
    NULL,
    "ild_msm_recovery",
    list(
      n_sim = n_sim,
      n_id = n_id,
      n_obs_per = n_obs_per,
      true_ate = true_ate,
      inference = inference,
      n_scenarios = nrow(scen)
    ),
    list(n_success = sum(sim_results$converged, na.rm = TRUE))
  )
  out
}

#' @export
print.ild_msm_recovery <- function(x, ...) {
  cat("MSM recovery summary\n")
  print(x$summary)
  invisible(x)
}

#' @keywords internal
#' @noRd
.ild_msm_build_scenario_grid <- function(scenario_grid, n_id, n_obs_per, true_ate, censoring) {
  if (is.null(scenario_grid)) {
    return(tibble::tibble(
      scenario_id = "baseline",
      n_id = as.integer(n_id),
      n_obs_per = as.integer(n_obs_per),
      true_ate = as.numeric(true_ate),
      censoring = isTRUE(censoring),
      positivity_stress = 1,
      treatment_strength = 0.8,
      censoring_strength = 0.6,
      misspec_treatment_model = FALSE
    ))
  }
  scen <- tibble::as_tibble(scenario_grid)
  if (!"scenario_id" %in% names(scen)) {
    scen$scenario_id <- paste0("scenario_", seq_len(nrow(scen)))
  }
  defaults <- list(
    n_id = as.integer(n_id),
    n_obs_per = as.integer(n_obs_per),
    true_ate = as.numeric(true_ate),
    censoring = isTRUE(censoring),
    positivity_stress = 1,
    treatment_strength = 0.8,
    censoring_strength = 0.6,
    misspec_treatment_model = FALSE
  )
  for (nm in names(defaults)) {
    if (!nm %in% names(scen)) scen[[nm]] <- defaults[[nm]]
  }
  scen
}

#' @keywords internal
#' @noRd
.ild_msm_propensity_overlap_summary <- function(data) {
  fits <- attr(data, "ild_iptw_msm_fits", exact = TRUE)
  if (is.null(fits) || !length(fits)) {
    return(list(extreme_rate = NA_real_))
  }
  pv <- unlist(lapply(fits, function(f) tryCatch(stats::fitted(f), error = function(e) numeric())), use.names = FALSE)
  pv <- pv[is.finite(pv)]
  if (!length(pv)) return(list(extreme_rate = NA_real_))
  list(extreme_rate = mean(pv < 0.05 | pv > 0.95, na.rm = TRUE))
}

#' @keywords internal
#' @noRd
.ild_msm_recovery_summary_by_scenario <- function(sim_results) {
  grouped <- dplyr::group_by(sim_results, .data$scenario_id, .data$true_ate)
  dplyr::summarise(
    grouped,
    n_sim = dplyr::n(),
    n_success = sum(.data$converged & is.finite(.data$estimate), na.rm = TRUE),
    failure_rate = mean(!.data$converged, na.rm = TRUE),
    bias = mean(.data$estimate - .data$true_ate, na.rm = TRUE),
    rmse = sqrt(mean((.data$estimate - .data$true_ate)^2, na.rm = TRUE)),
    coverage = mean(.data$covered, na.rm = TRUE),
    ess_mean = mean(.data$ess, na.rm = TRUE),
    ess_min = min(.data$ess, na.rm = TRUE),
    ess_p10 = stats::quantile(.data$ess, probs = 0.10, na.rm = TRUE, type = 7),
    ess_p50 = stats::quantile(.data$ess, probs = 0.50, na.rm = TRUE, type = 7),
    ess_p90 = stats::quantile(.data$ess, probs = 0.90, na.rm = TRUE, type = 7),
    weight_ratio_median = stats::median(.data$weight_ratio, na.rm = TRUE),
    propensity_extreme_rate = mean(.data$propensity_extreme_rate, na.rm = TRUE),
    inference_degraded_rate = mean(.data$inference_status != "ok", na.rm = TRUE),
    inference_unsupported_rate = mean(.data$inference_status == "unsupported", na.rm = TRUE),
    inference_method_modal = names(sort(table(.data$inference_method), decreasing = TRUE))[1L],
    mean_ci_width = mean(.data$conf_high - .data$conf_low, na.rm = TRUE),
    coverage_calibration_gap = mean(.data$covered, na.rm = TRUE) - 0.95,
    .groups = "drop"
  )
}

#' @keywords internal
#' @noRd
.ild_msm_recovery_summary_overall <- function(summary_by_scenario, n_sim) {
  tibble::tibble(
    n_sim = n_sim,
    n_scenarios = nrow(summary_by_scenario),
    n_success = sum(summary_by_scenario$n_success, na.rm = TRUE),
    bias = mean(summary_by_scenario$bias, na.rm = TRUE),
    rmse = mean(summary_by_scenario$rmse, na.rm = TRUE),
    coverage = mean(summary_by_scenario$coverage, na.rm = TRUE),
    ess_mean = mean(summary_by_scenario$ess_mean, na.rm = TRUE),
    ess_min = min(summary_by_scenario$ess_min, na.rm = TRUE),
    ess_p10 = mean(summary_by_scenario$ess_p10, na.rm = TRUE),
    ess_p50 = mean(summary_by_scenario$ess_p50, na.rm = TRUE),
    ess_p90 = mean(summary_by_scenario$ess_p90, na.rm = TRUE),
    weight_ratio_median = stats::median(summary_by_scenario$weight_ratio_median, na.rm = TRUE),
    inference_degraded_rate = mean(summary_by_scenario$inference_degraded_rate, na.rm = TRUE),
    inference_unsupported_rate = mean(summary_by_scenario$inference_unsupported_rate, na.rm = TRUE),
    coverage_calibration_gap = mean(summary_by_scenario$coverage_calibration_gap, na.rm = TRUE)
  )
}
