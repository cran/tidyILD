# Internal helpers for ild_augment() schema (ILD_AUGMENT_REQUIRED_COLS / optional)

#' Principled Pearson residuals for augment (sparse: NA if unavailable or length mismatch)
#' @keywords internal
#' @noRd
ild_augment_pearson_residuals <- function(fit, n_expected) {
  out <- rep(NA_real_, n_expected)
  r <- tryCatch(
    stats::residuals(fit, type = "pearson"),
    error = function(e) NULL
  )
  if (is.null(r)) return(out)
  r <- as.numeric(unname(as.vector(r)))
  if (length(r) != n_expected) return(out)
  r
}

#' Build augmented tibble: required columns + optional schema columns
#' @keywords internal
#' @noRd
ild_augment_assemble <- function(.ild_id,
                                 .ild_time,
                                 .outcome,
                                 .fitted,
                                 .resid,
                                 .resid_std,
                                 engine,
                                 model_class,
                                 optional = NULL) {
  n <- length(.ild_id)
  out <- tibble::tibble(
    .ild_id = .ild_id,
    .ild_time = .ild_time,
    .outcome = .outcome,
    .fitted = .fitted,
    .resid = .resid,
    .resid_std = .resid_std,
    engine = engine,
    model_class = model_class
  )
  for (nm in ILD_AUGMENT_OPTIONAL_COLS) {
    if (!is.null(optional) && !is.null(optional[[nm]])) {
      out[[nm]] <- optional[[nm]]
    } else {
      out[[nm]] <- rep(NA_real_, n)
    }
  }
  out
}
