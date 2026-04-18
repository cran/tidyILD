#' Simulation-based power analysis for a fixed effect in ILD models
#'
#' Estimates empirical power by repeatedly simulating data with a known effect
#' (via [ild_simulate()] plus one added predictor), fitting with [ild_lme()],
#' and counting the proportion of runs where the target term is significant
#' (Wald p < alpha). The workflow (simulate, fit, reject/retain) mirrors
#' simulation-based power in packages like mixpower; \code{ild_power()} is
#' focused on ILD and \code{ild_lme()}. For multi-parameter grids, LRT, or
#' general LMMs, consider mixpower.
#'
#' The data-generating process adds one predictor (name from \code{test_term})
#' as standard normal and adds \code{effect_size * predictor} to the outcome
#' on top of the base [ild_simulate()] DGP (id, time, y). No change to
#' \code{ild_simulate()} is required.
#'
#' For \code{ar1 = FALSE} (lmer), the lme4 backend does not report p-values;
#' inference for the test term uses a Wald z-approximation (estimate / SE)
#' so that power is still computed. For \code{ar1 = TRUE} (nlme), p-values
#' come from the model summary.
#'
#' @param formula Fixed-effects formula including the predictor to power for
#'   and random effects, e.g. \code{y ~ x + (1 | id)}. For \code{ar1 = TRUE},
#'   use a fixed-only formula (random passed internally).
#' @param n_sim Integer. Number of simulation replications (default 500).
#' @param n_id Integer. Number of persons per replication.
#' @param n_obs_per Integer. Observations per person per replication.
#' @param effect_size Numeric. True coefficient for \code{test_term} in the DGP.
#' @param test_term Character or \code{NULL}. Which fixed-effect term to test.
#'   If \code{NULL}, taken as the first non-intercept fixed-effect term from
#'   the model (inferred from the formula).
#' @param alpha Numeric. Significance level for rejection (default 0.05).
#' @param ar1 Logical. If \code{TRUE}, fit with nlme and residual AR1/CAR1
#'   (default \code{FALSE}).
#' @param seed Integer. Base random seed; replication \code{i} uses \code{seed + i}.
#' @param return_sims Logical. If \code{TRUE}, include a tibble of per-run
#'   estimate, std_error, p_value, rejected in the result (default \code{FALSE}).
#' @param verbose Logical. If \code{TRUE}, message progress (default \code{TRUE}).
#' @param ... Passed to [ild_simulate()] (e.g. \code{irregular}, \code{wp_effect},
#'   \code{bp_effect}) and to [ild_lme()].
#' @return A list: \code{power} (proportion of converged runs with p < alpha),
#'   \code{n_sim}, \code{n_reject}, \code{n_converged}, \code{n_failed},
#'   \code{alpha}, \code{test_term}. If \code{return_sims = TRUE}, also
#'   \code{sim_results} (tibble of per-run results).
#' @seealso [ild_recovery_metrics()] to summarize bias, RMSE, and coverage from
#'   \code{return_sims}. \href{../doc/benchmark-simulation-recovery.html}{\code{vignette("benchmark-simulation-recovery", package = "tidyILD")}}
#'   (worked example).
#' @examples
#' set.seed(42)
#' res <- ild_power(
#'   formula = y ~ x + (1 | id),
#'   n_sim = 25L,
#'   n_id = 15L,
#'   n_obs_per = 10L,
#'   effect_size = 0.3,
#'   seed = 42L,
#'   verbose = FALSE
#' )
#' res$power
#' res$n_reject
#' @importFrom stats rnorm
#' @export
ild_power <- function(formula,
                     n_sim = 500L,
                     n_id,
                     n_obs_per,
                     effect_size,
                     test_term = NULL,
                     alpha = 0.05,
                     ar1 = FALSE,
                     seed = 42L,
                     return_sims = FALSE,
                     verbose = TRUE,
                     ...) {
  formula <- stats::as.formula(formula)
  n_sim <- as.integer(n_sim)[1]
  if (missing(n_id)) stop("n_id is required.", call. = FALSE)
  if (missing(n_obs_per)) stop("n_obs_per is required.", call. = FALSE)
  n_id <- as.integer(n_id)[1]
  n_obs_per <- as.integer(n_obs_per)[1]
  if (missing(effect_size)) stop("effect_size is required.", call. = FALSE)
  effect_size <- as.numeric(effect_size)[1]
  alpha <- as.numeric(alpha)[1]
  seed <- as.integer(seed)[1]

  # Resolve test_term: first non-intercept fixed-effect variable from formula
  all_v <- all.vars(formula)
  if (length(all_v) < 2L) stop("formula must include at least one predictor.", call. = FALSE)
  rhs_vars <- all_v[-1L]
  # Exclude grouping factor (id) so that y ~ 1 + (1|id) has no fixed predictor
  fixed_vars <- setdiff(rhs_vars, c("id"))
  if (length(fixed_vars) == 0L) stop("formula must include at least one predictor.", call. = FALSE)
  if (is.null(test_term)) {
    test_term <- fixed_vars[1L]
  }
  test_term <- as.character(test_term)[1L]

  # ... is passed only to ild_simulate (irregular, wp_effect, bp_effect, etc.)
  dots <- list(...)
  sim_args <- c(list(n_id = n_id, n_obs_per = n_obs_per), dots)

  n_reject <- 0L
  n_failed <- 0L
  sim_results <- if (return_sims) {
    list(estimate = numeric(n_sim), std_error = numeric(n_sim),
         p_value = numeric(n_sim), rejected = logical(n_sim), converged = logical(n_sim))
  } else {
    NULL
  }

  for (i in seq_len(n_sim)) {
    set.seed(seed + i)
    # Generate base data (ild_simulate does not add the predictor)
    sim_args$seed <- seed + i
    d <- do.call(ild_simulate, sim_args)
    # Add predictor and true effect
    d[[test_term]] <- stats::rnorm(nrow(d))
    d$y <- d$y + effect_size * d[[test_term]]
    # Prepare and fit
    prep <- tryCatch(
      ild_prepare(d, id = "id", time = "time"),
      error = function(e) NULL
    )
    if (is.null(prep)) {
      n_failed <- n_failed + 1L
      if (return_sims) {
        sim_results$estimate[i] <- NA_real_
        sim_results$std_error[i] <- NA_real_
        sim_results$p_value[i] <- NA_real_
        sim_results$rejected[i] <- FALSE
        sim_results$converged[i] <- FALSE
      }
      next
    }
    fit <- tryCatch(
      suppressWarnings(ild_lme(
        formula, prep,
        ar1 = ar1,
        warn_no_ar1 = FALSE,
        warn_uncentered = FALSE
      )),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      n_failed <- n_failed + 1L
      if (return_sims) {
        sim_results$estimate[i] <- NA_real_
        sim_results$std_error[i] <- NA_real_
        sim_results$p_value[i] <- NA_real_
        sim_results$rejected[i] <- FALSE
        sim_results$converged[i] <- FALSE
      }
      next
    }
    tbl <- tryCatch(tidy_ild_model(fit, object = FALSE), error = function(e) NULL)
    if (is.null(tbl)) {
      n_failed <- n_failed + 1L
      if (return_sims) {
        sim_results$estimate[i] <- NA_real_
        sim_results$std_error[i] <- NA_real_
        sim_results$p_value[i] <- NA_real_
        sim_results$rejected[i] <- FALSE
        sim_results$converged[i] <- FALSE
      }
      next
    }
    row <- tbl[tbl$term == test_term, ]
    if (nrow(row) != 1L) {
      n_failed <- n_failed + 1L
      if (return_sims) {
        sim_results$estimate[i] <- NA_real_
        sim_results$std_error[i] <- NA_real_
        sim_results$p_value[i] <- NA_real_
        sim_results$rejected[i] <- FALSE
        sim_results$converged[i] <- FALSE
      }
      next
    }
    pval <- row$p_value[1L]
    if (is.na(pval)) {
      # lme4 does not report p-values; use Wald z-approximation for power
      est <- row$estimate[1L]
      se <- row$std_error[1L]
      if (!is.na(se) && se > 0) pval <- 2 * (1 - stats::pnorm(abs(est / se)))
    }
    rej <- !is.na(pval) && pval < alpha
    if (rej) n_reject <- n_reject + 1L
    if (return_sims) {
      sim_results$estimate[i] <- row$estimate[1L]
      sim_results$std_error[i] <- row$std_error[1L]
      sim_results$p_value[i] <- pval
      sim_results$rejected[i] <- rej
      sim_results$converged[i] <- TRUE
    }
    if (verbose && (i %% 100L == 0L)) message("ild_power: ", i, "/", n_sim, " replications.")
  }

  n_converged <- n_sim - n_failed
  power <- if (n_converged > 0L) n_reject / n_converged else NA_real_

  out <- list(
    power = power,
    n_sim = n_sim,
    n_reject = n_reject,
    n_converged = n_converged,
    n_failed = n_failed,
    alpha = alpha,
    test_term = test_term
  )
  if (return_sims && !is.null(sim_results)) {
    out$sim_results <- tibble::tibble(
      sim = seq_len(n_sim),
      estimate = sim_results$estimate,
      std_error = sim_results$std_error,
      p_value = sim_results$p_value,
      rejected = sim_results$rejected,
      converged = sim_results$converged
    )
  }
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(NULL, "ild_power", list(
    formula = deparse(formula),
    n_sim = n_sim,
    n_id = n_id,
    n_obs_per = n_obs_per,
    effect_size = effect_size,
    test_term = test_term,
    alpha = alpha,
    ar1 = ar1
  ), list(n_reject = n_reject, n_converged = n_converged, n_failed = n_failed, power = power))
  out
}
