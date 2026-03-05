#' Autocorrelation function for ILD variables or model residuals
#'
#' Computes ACF on a variable in ILD data or on residuals from an [ild_lme()] fit.
#' Use this to check whether AR1 is appropriate before fitting models. ACF is
#' computed over the ordered observation sequence (pooled or within person); it
#' does not adjust for irregular time gaps.
#'
#' @param x Either an ILD object (see [is_ild()]) or a fitted model from [ild_lme()].
#' @param ... When \code{x} is ILD data, the variable(s) to compute ACF on (tidy-select; one variable). Ignored when \code{x} is a fit.
#' @param by_id Logical. If \code{TRUE}, also return per-person ACF in \code{acf_by_id} (default \code{FALSE}).
#' @return A list with \code{acf}: a tibble with columns \code{lag} and \code{acf} (pooled). If \code{by_id = TRUE}, \code{acf_by_id} is a named list of tibbles (one per person).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 5, n_obs_per = 10, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' ild_acf(x, "y")
#' fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
#' ild_acf(fit)
#' @importFrom dplyr select
ild_acf <- function(x, ..., by_id = FALSE) {
  is_fit <- inherits(x, "lmerMod") || inherits(x, "lme")
  if (is_fit && !is.null(attr(x, "ild_data", exact = TRUE))) {
    data_ref <- augment_ild_model(x)
    id_col <- ".ild_id"
    res <- data_ref$.resid
    ids <- unique(data_ref[[id_col]])
  } else if (is_fit) {
    stop("Fit was not produced by tidyILD; refit using ild_lme() so the fit carries ild_data.",
         call. = FALSE)
  } else {
    validate_ild(x)
    if (length(list(...)) == 0) stop("No variable selected; provide a variable when x is ILD data (e.g. ild_acf(x, mood)).", call. = FALSE)
    data_ref <- x
    id_col <- ".ild_id"
    vars <- names(dplyr::select(x, ...))
    if (length(vars) == 0) stop("No variable selected; provide a variable when x is ILD data.", call. = FALSE)
    if (length(vars) > 1) stop("Select a single variable for ACF.", call. = FALSE)
    res <- x[[vars]]
    ids <- unique(x[[id_col]])
  }
  pooled_tbl <- ild_acf_vec(res[!is.na(res)])
  out <- list(acf = pooled_tbl)
  if (by_id) {
    by_id_tbls <- vector("list", length(ids))
    names(by_id_tbls) <- as.character(ids)
    for (i in seq_along(ids)) {
      r <- res[data_ref[[id_col]] == ids[i]]
      r <- r[!is.na(r)]
      by_id_tbls[[i]] <- ild_acf_vec(r)
    }
    out$acf_by_id <- by_id_tbls
  }
  out
}

#' ACF of a numeric vector as tibble (lag, acf)
#' @noRd
ild_acf_vec <- function(v) {
  if (length(v) <= 2) return(tibble::tibble(lag = numeric(), acf = numeric()))
  acf_obj <- stats::acf(v, plot = FALSE, na.action = stats::na.pass)
  tibble::tibble(lag = as.vector(acf_obj$lag), acf = as.vector(acf_obj$acf))
}
