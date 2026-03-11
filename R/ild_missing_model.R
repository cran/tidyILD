#' Fit a model for missingness (diagnostic / sensitivity)
#'
#' Fits a logistic model predicting whether the outcome is missing from
#' covariates. Use as a diagnostic; then [ild_ipw_weights()] to compute
#' inverse-probability weights and [ild_ipw_refit()] for a sensitivity analysis.
#' This is not a full MNAR solution—treat as diagnostic and sensitivity tooling.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param outcome Character. Name of the variable with missingness (e.g. \code{"mood"}).
#' @param predictors Character vector. Names of covariates to predict missingness.
#' @param random Logical. If \code{TRUE}, fit \code{glmer(is_missing ~ ... + (1|id))};
#'   if \code{FALSE} (default), fit \code{glm(is_missing ~ ., family = binomial)}.
#' @param family Passed to \code{glm} / \code{glmer} (default \code{binomial()}).
#' @param ... Passed to \code{glm()} or \code{lme4::glmer()}.
#' @return A list with \code{fit} (the fitted model or \code{NULL} if no/all missing),
#'   \code{tidy} (tibble: term, estimate, std_error, p_value), \code{outcome},
#'   \code{predictors}, and \code{message}. If \code{fit} is not \code{NULL},
#'   \code{p_missing} is a numeric vector of predicted P(missing) per row (aligned to \code{x}).
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 15, n_obs_per = 8, seed = 1)
#' d$stress <- rnorm(nrow(d))
#' d$mood <- d$y
#' d$mood[sample(nrow(d), 20)] <- NA
#' x <- ild_prepare(d, id = "id", time = "time")
#' mm <- ild_missing_model(x, "mood", "stress")
#' mm$tidy
ild_missing_model <- function(x, outcome, predictors, random = FALSE,
                              family = stats::binomial(), ...) {
  validate_ild(x)
  nms <- names(x)
  if (!outcome %in% nms) stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  for (p in predictors) {
    if (!p %in% nms) stop("predictor '", p, "' not found in data.", call. = FALSE)
  }
  is_missing <- as.integer(is.na(x[[outcome]]))
  n_miss <- sum(is_missing)
  n_obs <- sum(!is_missing)
  miss_args <- list(outcome = outcome, predictors = predictors, random = random)
  miss_outputs <- list(n_missing = n_miss, n_obs = n_obs)
  if (n_miss == 0L) {
    out <- list(
      fit = NULL,
      tidy = tibble::tibble(term = character(), estimate = double(), std_error = double(), p_value = double()),
      outcome = outcome,
      predictors = predictors,
      message = "No missingness in outcome; nothing to model."
    )
    attr(out, "ild_provenance") <- ild_new_analysis_provenance(x, "ild_missing_model", miss_args, miss_outputs)
    return(out)
  }
  if (n_obs == 0L) {
    out <- list(
      fit = NULL,
      tidy = tibble::tibble(term = character(), estimate = double(), std_error = double(), p_value = double()),
      outcome = outcome,
      predictors = predictors,
      message = "All outcome values are missing; cannot fit model."
    )
    attr(out, "ild_provenance") <- ild_new_analysis_provenance(x, "ild_missing_model", miss_args, miss_outputs)
    return(out)
  }
  id_col <- ".ild_id"
  df <- as.data.frame(x[, c(id_col, predictors), drop = FALSE])
  df$is_missing <- is_missing
  df$id <- df[[id_col]]
  form <- stats::as.formula(paste("is_missing ~", paste(predictors, collapse = " + ")))
  fit <- if (random && requireNamespace("lme4", quietly = TRUE)) {
    tryCatch(
      lme4::glmer(stats::update(form, . ~ . + (1 | id)), data = df, family = family, ...),
      error = function(e) stats::glm(form, data = df, family = family, ...)
    )
  } else {
    stats::glm(form, data = df, family = family, ...)
  }
  co <- summary(fit)$coefficients
  if (is.null(co) || nrow(co) == 0L) {
    tidy_tbl <- tibble::tibble(term = character(), estimate = double(), std_error = double(), p_value = double())
  } else {
    tidy_tbl <- tibble::tibble(
      term = rownames(co),
      estimate = as.vector(co[, "Estimate"]),
      std_error = as.vector(co[, "Std. Error"]),
      p_value = if ("Pr(>|z|)" %in% colnames(co)) as.vector(co[, "Pr(>|z|)"]) else rep(NA_real_, nrow(co))
    )
  }
  p_missing <- stats::predict(fit, type = "response")
  if (length(p_missing) != nrow(x)) {
    p_missing <- rep(NA_real_, nrow(x))
  }
  out <- list(
    fit = fit,
    tidy = tidy_tbl,
    outcome = outcome,
    predictors = predictors,
    p_missing = p_missing,
    message = "Missingness model fit; use ild_ipw_weights() for IPW and ild_ipw_refit() for sensitivity."
  )
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(x, "ild_missing_model", list(
    outcome = outcome,
    predictors = predictors,
    random = random
  ), list(n_missing = n_miss, n_obs = n_obs))
  out
}
