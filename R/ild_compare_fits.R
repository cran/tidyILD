# Compare fitted models on information criteria (and optional guardrail counts)

#' Compare fitted models (AIC, BIC, nobs)
#'
#' Builds a compact comparison table for a named list of fitted objects.
#' Uses [stats::AIC()], [stats::BIC()], and [stats::nobs()] where those methods
#' exist. This is **not** a likelihood-ratio test and does not establish that
#' models are nested; interpret only when model comparison is scientifically
#' appropriate for your estimand.
#'
#' @param fits A named (or unnamed) list of fitted model objects.
#' @param guardrail_bundles Optional named list of [ild_diagnostics_bundle] objects
#'   aligned with \code{fits} (same length and order). When provided, adds
#'   \code{n_guardrails} per row.
#' @return A tibble with columns \code{model}, \code{engine} (from [ild_methods()]
#'   truncated to one line when long), \code{aic}, \code{bic}, \code{n_obs},
#'   \code{converged} (best-effort), \code{n_guardrails} (if bundles given),
#'   and \code{notes} (fit-specific caveats).
#' @seealso \code{vignette("temporal-dynamics-model-choice", package = "tidyILD")}.
#' @export
ild_compare_fits <- function(fits, guardrail_bundles = NULL) {
  if (!is.list(fits) || length(fits) == 0L) {
    stop("fits must be a non-empty list.", call. = FALSE)
  }
  nm <- names(fits)
  if (is.null(nm)) {
    nm <- paste0("model", seq_along(fits))
  }
  if (any(!nzchar(nm))) {
    nm[!nzchar(nm)] <- paste0("model", which(!nzchar(nm)))
  }
  if (!is.null(guardrail_bundles) && length(guardrail_bundles) != length(fits)) {
    stop("guardrail_bundles must be NULL or have the same length as fits.", call. = FALSE)
  }
  rows <- vector("list", length(fits))
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    lab <- nm[i]
    eng <- tryCatch(as.character(ild_methods(fit))[1L], error = function(e) NA_character_)
    if (is.character(eng) && nchar(eng) > 120L) {
      eng <- paste0(substr(eng, 1L, 117L), "...")
    }
    aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
    bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
    nobs <- tryCatch(as.integer(stats::nobs(fit)), error = function(e) NA_integer_)
    conv <- ild_compare_fits_converged(fit)
    notes <- character(1L)
    if (inherits(fit, "lmerMod") && requireNamespace("lme4", quietly = TRUE)) {
      if (isTRUE(lme4::isSingular(fit))) {
        notes <- "lmer singular fit"
      }
    }
    n_gr <- NA_integer_
    if (!is.null(guardrail_bundles)) {
      b <- guardrail_bundles[[i]]
      if (!is.null(b) && inherits(b, "ild_diagnostics_bundle") && tibble::is_tibble(b$guardrails)) {
        n_gr <- as.integer(nrow(b$guardrails))
      }
    }
    rows[[i]] <- tibble::tibble(
      model = lab,
      engine = eng,
      aic = as.numeric(aic)[1L],
      bic = as.numeric(bic)[1L],
      n_obs = nobs,
      converged = conv,
      n_guardrails = n_gr,
      notes = notes
    )
  }
  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
ild_compare_fits_converged <- function(fit) {
  if (inherits(fit, "lmerMod")) {
    conv <- fit@optinfo$conv$lme4
    if (!is.null(conv$code) && is.numeric(conv$code)) {
      return(as.integer(conv$code) == 0L)
    }
    return(NA)
  }
  if (inherits(fit, "lme")) {
    return(TRUE)
  }
  if (inherits(fit, "brmsfit") && requireNamespace("brms", quietly = TRUE)) {
    rh <- tryCatch(max(brms::rhat(fit), na.rm = TRUE), error = function(e) NA_real_)
    return(is.finite(rh) && rh < 1.09)
  }
  if (inherits(fit, "gam")) {
    return(TRUE)
  }
  NA
}
