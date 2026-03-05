#' Test whether missingness is associated with a predictor (informative missingness)
#'
#' Fits a logistic model of missingness (binary: is the outcome NA?) on a
#' predictor variable. Use as a diagnostic: if the predictor is significant,
#' missingness may be informative and results could be biased. This function
#' does not correct for missingness; it flags the assumption for sensitivity analyses.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param outcome_var Character. Name of the variable with missingness (e.g. \code{"mood"}).
#' @param predictor_var Character. Name of the suspected predictor of missingness (e.g. \code{"stress"}).
#' @param random Logical. If \code{TRUE}, fit a mixed-effects logistic model
#'   \code{is_missing ~ predictor + (1 | id)} via \code{lme4::glmer}; if \code{FALSE}
#'   (default), fit \code{glm(is_missing ~ predictor, family = binomial)}.
#' @return A list with \code{predictor} (name), \code{estimate}, \code{std_error},
#'   \code{p_value}, and \code{message} (short note about informative missingness).
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 20, n_obs_per = 10, seed = 1)
#' d$stress <- rnorm(nrow(d))
#' d$mood <- d$y
#' d$mood[sample(nrow(d), 30)] <- NA  # some missing
#' x <- ild_prepare(d, id = "id", time = "time")
#' ild_missing_bias(x, "mood", "stress")
ild_missing_bias <- function(x, outcome_var, predictor_var, random = FALSE) {
  validate_ild(x)
  nms <- names(x)
  if (!outcome_var %in% nms) stop("outcome_var '", outcome_var, "' not found in data.", call. = FALSE)
  if (!predictor_var %in% nms) stop("predictor_var '", predictor_var, "' not found in data.", call. = FALSE)
  is_missing <- as.integer(is.na(x[[outcome_var]]))
  pred_vals <- x[[predictor_var]]
  if (!is.numeric(pred_vals)) stop("predictor_var must be numeric.", call. = FALSE)
  n_miss <- sum(is_missing)
  n_obs <- sum(!is_missing)
  if (n_miss == 0) {
    return(list(
      predictor = predictor_var,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      message = "No missingness in outcome; nothing to model."
    ))
  }
  if (n_obs == 0) {
    return(list(
      predictor = predictor_var,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      message = "All outcome values are missing; cannot fit model."
    ))
  }
  id_col <- ".ild_id"
  df <- data.frame(
    is_missing = is_missing,
    pred = pred_vals,
    id = x[[id_col]]
  )
  if (random && requireNamespace("lme4", quietly = TRUE)) {
    fit <- tryCatch(
      lme4::glmer(is_missing ~ pred + (1 | id), data = df, family = stats::binomial),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      fit_glm <- stats::glm(is_missing ~ pred, data = df, family = stats::binomial)
      co <- summary(fit_glm)$coefficients
      est <- co["pred", "Estimate"]
      se <- co["pred", "Std. Error"]
      pval <- co["pred", "Pr(>|z|)"]
    } else {
      co <- summary(fit)$coefficients
      est <- co["pred", "Estimate"]
      se <- co["pred", "Std. Error"]
      pval <- co["pred", "Pr(>|z|)"]
    }
  } else {
    fit_glm <- stats::glm(is_missing ~ pred, data = df, family = stats::binomial)
    co <- summary(fit_glm)$coefficients
    est <- co["pred", "Estimate"]
    se <- co["pred", "Std. Error"]
    pval <- co["pred", "Pr(>|z|)"]
  }
  msg <- if (!is.na(pval) && pval < 0.05) {
    paste0("Missingness may be associated with ", predictor_var,
           "; consider sensitivity analyses (e.g. multiple imputation or selection models).")
  } else {
    paste0("No strong evidence that missingness is associated with ", predictor_var, " (p = ", round(pval, 4), ").")
  }
  list(
    predictor = predictor_var,
    estimate = est,
    std_error = se,
    p_value = pval,
    message = msg
  )
}
