# Bayesian ILD models via brms (optional dependency: brms, rstan / cmdstanr)

#' Prior specifications for common ILD mixed models
#'
#' Returns a \code{brmsprior} object suitable for the \code{prior} argument of
#' [ild_brms()] / [brms::brm()]. Templates follow weakly informative defaults:
#' Student-t on intercepts and group SDs, normal on regression coefficients, and
#' exponential on residual SD where applicable.
#'
#' @param template Character selecting the template:
#'   \describe{
#'     \item{\code{default}}{Student-t intercept; Normal(0, 5) fixed effects;
#'       Student-t group SDs; exponential residual SD (Gaussian models).}
#'     \item{\code{weakly_informative}}{Wider scales (more diffuse) on intercept
#'       and \code{b} coefficients.}
#'     \item{\code{minimal_shrinkage}}{Tighter Normal(0, 2.5) on \code{b} (similar
#'       to Gelman et al. scaled logistic guidance; use with scaled predictors).}
#'   }
#' @return A \code{brmsprior} data frame (class \code{brmsprior}).
#' @seealso [ild_brms()]
#' @export
ild_prior_ild <- function(template = c("default", "weakly_informative", "minimal_shrinkage")) {
  rlang::check_installed("brms", reason = "to use ild_prior_ild()")
  template <- match.arg(template)
  switch(template,
    default = c(
      brms::set_prior("student_t(3, 0, 10)", class = "Intercept"),
      brms::set_prior("normal(0, 5)", class = "b"),
      brms::set_prior("student_t(3, 0, 2.5)", class = "sd"),
      brms::set_prior("exponential(1)", class = "sigma")
    ),
    weakly_informative = c(
      brms::set_prior("student_t(3, 0, 10)", class = "Intercept"),
      brms::set_prior("normal(0, 10)", class = "b"),
      brms::set_prior("student_t(3, 0, 5)", class = "sd"),
      brms::set_prior("exponential(1)", class = "sigma")
    ),
    minimal_shrinkage = c(
      brms::set_prior("student_t(3, 0, 5)", class = "Intercept"),
      brms::set_prior("normal(0, 2.5)", class = "b"),
      brms::set_prior("student_t(3, 0, 2.5)", class = "sd"),
      brms::set_prior("exponential(1)", class = "sigma")
    )
  )
}

#' Build standardized posterior / sampler metadata (internal)
#' @param fit A \code{brmsfit} object.
#' @param prior_template Character name of template if used, else \code{NULL}.
#' @noRd
ild_posterior_attr <- function(fit, prior_template = NULL) {
  sm <- summary(fit)
  ca <- tryCatch(fit$fit@stan_args[[1L]], error = function(e) NULL)
  adapt_delta <- if (!is.null(ca$adapt_delta)) ca$adapt_delta else NA_real_
  max_td <- if (!is.null(ca$max_treedepth)) as.integer(ca$max_treedepth) else NA_integer_
  seed <- if (!is.null(ca$seed)) ca$seed else NA_integer_
  ps_df <- tryCatch(
    as.data.frame(brms::prior_summary(fit)),
    error = function(e) NULL
  )
  list(
    engine = "brms",
    backend = "brms",
    brms_version = ild_backend_pkg_version("brms"),
    rstan_version = ild_backend_pkg_version("rstan"),
    prior_template = prior_template,
    prior_summary = ps_df,
    chains = sm$chains,
    iter = sm$iter,
    warmup = sm$warmup,
    thin = sm$thin,
    total_post_warmup_draws = sm$total_ndraws,
    sampler = sm$sampler,
    algorithm = sm$algorithm,
    adapt_delta = adapt_delta,
    max_treedepth = max_td,
    seed = seed,
    n_divergent = ild_brms_n_divergent(fit),
    n_max_treedepth_hits = ild_brms_n_treedepth_hits(fit, max_td)
  )
}

#' @noRd
ild_brms_n_divergent <- function(fit) {
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  if (is.null(np) || !all(c("Parameter", "Value") %in% names(np))) return(NA_integer_)
  div <- np[np$Parameter == "divergent__", , drop = FALSE]
  if (nrow(div) == 0L) return(0L)
  sum(as.integer(div$Value))
}

#' Count post-warmup iterations hitting max treedepth (heuristic)
#' @noRd
ild_brms_n_treedepth_hits <- function(fit, max_td = NA_integer_) {
  np <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  if (is.null(np) || !all(c("Parameter", "Value") %in% names(np))) return(NA_integer_)
  td <- np[np$Parameter == "treedepth__", , drop = FALSE]
  if (nrow(td) == 0L) return(NA_integer_)
  if (is.na(max_td) || max_td <= 0L) max_td <- 10L
  sum(as.integer(td$Value) >= max_td)
}

#' Fit a Bayesian mixed model to ILD data with \pkg{brms}
#'
#' Wraps [brms::brm()] with ILD validation, default priors for common longitudinal
#' mixed models, standardized posterior metadata in \code{attr(fit, "ild_posterior")},
#' and analysis provenance (priors, chains, iterations). Attach \code{ild_data} for
#' use with [ild_tidy()], [ild_augment()], [ild_diagnose()], and [ild_methods()].
#'
#' Requires packages \pkg{brms} and a Stan backend (\pkg{rstan} or \pkg{cmdstanr}).
#'
#' @param formula A \pkg{brms} / \pkg{lme4}-style formula (e.g. \code{y ~ x + (1|id)}).
#' @param data An ILD object from [ild_prepare()].
#' @param prior Optional \code{brmsprior} object. If \code{NULL}, built from \code{prior_template}.
#' @param prior_template Passed to [ild_prior_ild()] when \code{prior} is \code{NULL}.
#' @param warn_uncentered If \code{TRUE} (default), warn when a numeric predictor varies
#'   within and between persons without \code{_wp}/\code{_bp} (same idea as [ild_lme()]).
#' @param ... Arguments passed to [brms::brm()] (e.g. \code{chains}, \code{iter},
#'   \code{warmup}, \code{control}, \code{backend}, \code{seed}).
#' @return A \code{brmsfit} object with attributes:
#'   \describe{
#'     \item{\code{ild_data}}{The ILD data frame.}
#'     \item{\code{ild_posterior}}{List: sampler settings, prior summary, divergences, etc.}
#'     \item{\code{ild_provenance}}{Standard tidyILD analysis provenance.}
#'   }
#' @seealso [ild_fit()] with \code{backend = "brms"} is equivalent to \code{ild_brms()}.
#'   For IPW/MSM workflows, full Bayesian joint MSM is not in tidyILD; see [ild_msm_inference].
#' @importFrom stats formula
#' @export
ild_brms <- function(formula,
                     data,
                     prior = NULL,
                     prior_template = c("default", "weakly_informative", "minimal_shrinkage"),
                     warn_uncentered = TRUE,
                     ...) {
  rlang::check_installed("brms", reason = "to use ild_brms()")
  validate_ild(data)
  prior_template <- match.arg(prior_template)
  if (is.null(prior)) {
    prior <- ild_prior_ild(prior_template)
    pt_name <- prior_template
  } else {
    pt_name <- NULL
  }
  if (warn_uncentered) {
    f_unc <- tryCatch(stats::formula(formula), error = function(e) formula)
    uncentered <- ild_detect_uncentered_predictors(data, f_unc)
    if (length(uncentered) > 0) {
      warning("Predictor(s) '", paste(uncentered, collapse = "', '"),
              "' vary both within and between persons. Consider using ild_center(",
              paste(uncentered, collapse = ", "), ") for within/between decomposition.",
              call. = FALSE)
    }
  }
  fit <- brms::brm(formula = formula, data = data, prior = prior, ...)
  attr(fit, "ild_data") <- data
  attr(fit, "ild_posterior") <- ild_posterior_attr(fit, prior_template = pt_name)
  ps <- attr(fit, "ild_posterior", exact = TRUE)
  prior_txt <- tryCatch(
    paste(utils::capture.output(print(brms::prior_summary(fit))), collapse = "\n"),
    error = function(e) "prior_summary unavailable"
  )
  attr(fit, "ild_provenance") <- ild_new_analysis_provenance(
    data, "ild_brms",
    list(
      formula = safe_deparse_formula(formula),
      prior_template = pt_name,
      prior_summary_text = prior_txt,
      chains = ps$chains,
      iter = ps$iter,
      warmup = ps$warmup,
      thin = ps$thin,
      adapt_delta = ps$adapt_delta,
      max_treedepth = ps$max_treedepth,
      seed = ps$seed,
      backend = "brms"
    ),
    list(
      n_obs = nrow(data),
      n_id = length(unique(data[[".ild_id"]])),
      brms_version = ps$brms_version,
      n_divergent = ps$n_divergent,
      max_rhat = ild_brms_max_rhat(fit)
    )
  )
  fit
}

#' @noRd
safe_deparse_formula <- function(f) {
  paste(deparse(f), collapse = " ")
}

#' Maximum R-hat across model parameters (brms)
#' @noRd
ild_brms_max_rhat <- function(fit) {
  rh <- tryCatch(brms::rhat(fit), error = function(e) NULL)
  if (is.null(rh) || length(rh) == 0L) return(NA_real_)
  suppressWarnings(max(as.numeric(rh), na.rm = TRUE))
}
