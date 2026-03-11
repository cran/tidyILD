#' Fit a time-varying effects model (TVEM) for ILD
#'
#' Fits a GAM with a smooth in time and a time-varying coefficient for a
#' predictor using [mgcv::gam()]. Use [ild_tvem_plot()] to plot the
#' time-varying effect. Requires \code{.ild_time_num} (or a numeric time column).
#'
#' @param data An ILD object (see [is_ild()]).
#' @param outcome Character. Name of the outcome variable.
#' @param predictor Character. Name of the predictor with a time-varying effect.
#' @param time_var Character. Name of the time variable (default \code{".ild_time_num"}).
#' @param k Integer. Basis dimension for smooth terms (default 10).
#' @param re_id Logical. If \code{TRUE} (default), include a random intercept by person (\code{s(.ild_id, bs="re")}).
#' @param ... Passed to [mgcv::gam()].
#' @return A fitted \code{gam} object with class \code{c("tidyild_tvem", "gam", ...)}
#'   and attribute \code{ild_tvem_meta} (list with \code{outcome}, \code{predictor},
#'   \code{time_var}, \code{k}, \code{re_id}).
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 10, n_obs_per = 15, seed = 1)
#' d$x <- rnorm(nrow(d))
#' x <- ild_prepare(d, id = "id", time = "time")
#' tv <- ild_tvem(x, "y", "x", k = 5, re_id = TRUE)
#' ild_tvem_plot(tv)
ild_tvem <- function(data, outcome, predictor, time_var = ".ild_time_num",
                     k = 10L, re_id = TRUE, ...) {
  validate_ild(data)
  nms <- names(data)
  if (!outcome %in% nms) stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  if (!predictor %in% nms) stop("predictor '", predictor, "' not found in data.", call. = FALSE)
  if (!time_var %in% nms) stop("time_var '", time_var, "' not found in data.", call. = FALSE)
  if (!is.numeric(data[[time_var]])) stop("time_var must be numeric.", call. = FALSE)
  if (!is.numeric(data[[predictor]])) stop("predictor must be numeric.", call. = FALSE)
  k <- as.integer(k)[1L]
  if (k < 3L) k <- 3L
  dat <- as.data.frame(data)
  if (re_id && ".ild_id" %in% nms) {
    dat$.ild_id <- factor(dat[[".ild_id"]])
    form <- stats::as.formula(paste0(
      outcome, " ~ s(", time_var, ", k = ", k, ") + s(", time_var,
      ", by = ", predictor, ", k = ", k, ") + s(.ild_id, bs = 're')"
    ))
  } else {
    form <- stats::as.formula(paste0(
      outcome, " ~ s(", time_var, ", k = ", k, ") + s(", time_var,
      ", by = ", predictor, ", k = ", k, ")"
    ))
  }
  fit <- mgcv::gam(form, data = dat, ...)
  attr(fit, "ild_tvem_meta") <- list(
    outcome = outcome,
    predictor = predictor,
    time_var = time_var,
    k = k,
    re_id = re_id
  )
  attr(fit, "ild_provenance") <- ild_new_analysis_provenance(data, "ild_tvem", list(
    outcome = outcome,
    predictor = predictor,
    time_var = time_var,
    k = k,
    re_id = re_id
  ), list(n_obs = nrow(data)))
  class(fit) <- c("tidyild_tvem", class(fit))
  fit
}
