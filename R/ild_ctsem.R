# Continuous-time state-space models via ctsem (optional dependency)

#' Fit a continuous-time state-space model via ctsem
#'
#' First-class tidyILD wrapper for \pkg{ctsem}. This backend is designed for
#' irregularly spaced ILD where continuous-time dynamics are preferable to
#' discrete-time approximations.
#'
#' v1 focuses on a conservative single-outcome pathway with explicit class,
#' provenance, and diagnostics compatibility. Users may supply a custom ctsem
#' model object; otherwise a simple 1-manifest local-level style template is used.
#'
#' @param data An object that passes [validate_ild()] (typically after [ild_prepare()]).
#' @param outcome Character name of numeric outcome column.
#' @param ct_model Optional ctsem model object. If \code{NULL}, a conservative
#'   default model is created: for \code{model_type = "stanct"}, \code{ctsem::ctModel(type = "ct", ...)}
#'   with one manifest loading on one latent; for \code{"ctfit"}, \code{type = "omx"} with
#'   \code{Tpoints = max(rows per id, 2)} from the prepared long data (custom \code{ct_model}
#'   may still be needed for irregular designs).
#' @param model_type Character. \code{"stanct"} (default) or \code{"ctfit"}.
#' @param id_col Person-id column in \code{data} (default \code{".ild_id"}).
#' @param time_col Continuous-time index column in \code{data}
#'   (default \code{".ild_time_num"}).
#' @param time_scale Optional positive scalar to rescale \code{time_col}
#'   before fitting (default \code{1}).
#' @param ... Passed to \code{ctsem::ctStanFit()} or \code{ctsem::ctFit()}.
#' @return An object of class \code{c("ild_fit_ctsem", "ild_fit_model", "ild_analysis")}.
#'   Attributes \code{ild_data} and \code{ild_provenance} are attached.
#' @seealso [ild_tidy()], [ild_augment()], [ild_diagnose()], [ild_autoplot()].
#' @export
ild_ctsem <- function(data,
                      outcome,
                      ct_model = NULL,
                      model_type = c("stanct", "ctfit"),
                      id_col = ".ild_id",
                      time_col = ".ild_time_num",
                      time_scale = 1,
                      ...) {
  rlang::check_installed("ctsem", reason = "to use ild_ctsem()")
  validate_ild(data)
  model_type <- match.arg(model_type)
  if (missing(outcome) || !nzchar(as.character(outcome)[1L])) {
    stop("outcome must be a non-empty column name.", call. = FALSE)
  }
  outcome <- as.character(outcome)[1L]
  id_col <- as.character(id_col)[1L]
  time_col <- as.character(time_col)[1L]
  if (!id_col %in% names(data)) {
    stop("id_col '", id_col, "' not found in data.", call. = FALSE)
  }
  if (!time_col %in% names(data)) {
    stop("time_col '", time_col, "' not found in data.", call. = FALSE)
  }
  if (!outcome %in% names(data)) {
    stop("outcome column '", outcome, "' not found in data.", call. = FALSE)
  }
  time_scale <- as.numeric(time_scale)[1L]
  if (!is.finite(time_scale) || time_scale <= 0) {
    stop("time_scale must be a positive finite scalar.", call. = FALSE)
  }

  dat <- data[, c(id_col, time_col, outcome), drop = FALSE]
  names(dat) <- c("id", "time", "y")
  dat$y <- suppressWarnings(as.numeric(dat$y))
  dat$time <- suppressWarnings(as.numeric(dat$time)) / time_scale
  if (any(!is.finite(dat$time))) {
    stop("time column contains non-finite values after scaling.", call. = FALSE)
  }
  if (sum(is.finite(dat$y)) < 5L) {
    stop("Need at least 5 finite outcome values to fit ctsem model.", call. = FALSE)
  }
  dat <- dat[is.finite(dat$y), , drop = FALSE]

  if (is.null(ct_model)) {
    max_tp <- max(2L, as.integer(max(table(dat$id))))
    ct_model <- .ild_ctsem_default_model(
      manifest_name = "y",
      model_type = model_type,
      max_Tpoints = max_tp
    )
  }

  fit <- .ild_ctsem_fit_dispatch(
    dat = dat,
    ct_model = ct_model,
    model_type = model_type,
    dots = list(...)
  )

  out <- list(
    ct_fit = fit$fit,
    fit_call = fit$fit_call,
    fit_type = fit$fit_type,
    ct_model = ct_model,
    outcome = outcome,
    id_col = id_col,
    time_col = time_col,
    time_scale = time_scale,
    n_obs = nrow(dat),
    n_id = length(unique(dat$id))
  )
  class(out) <- c("ild_fit_ctsem", "ild_fit_model", "ild_analysis")

  attr(out, "ild_data") <- data
  attr(out, "ild_response") <- outcome
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(
    data,
    "ild_ctsem",
    args = list(
      outcome = outcome,
      model_type = fit$fit_type,
      id_col = id_col,
      time_col = time_col,
      time_scale = time_scale
    ),
    outputs = list(
      n_obs = nrow(dat),
      n_id = length(unique(dat$id)),
      converged = .ild_ctsem_is_converged(out),
      ctsem_version = tryCatch(as.character(utils::packageVersion("ctsem")), error = function(e) NA_character_)
    )
  )
  out
}

#' @export
print.ild_fit_ctsem <- function(x, ...) {
  cat("tidyILD ctsem fit\n")
  cat("  outcome: ", x$outcome, "\n", sep = "")
  cat("  fit_type: ", x$fit_type, "\n", sep = "")
  cat("  n_obs: ", x$n_obs, "\n", sep = "")
  cat("  n_id: ", x$n_id, "\n", sep = "")
  cat("  converged: ", .ild_ctsem_is_converged(x), "\n", sep = "")
  invisible(x)
}

#' @keywords internal
#' @noRd
.ild_ctsem_default_model <- function(manifest_name = "y",
                                      model_type = c("stanct", "ctfit"),
                                      max_Tpoints = 2L) {
  model_type <- match.arg(model_type)
  lam <- matrix(1, nrow = 1L, ncol = 1L)
  if (identical(model_type, "stanct")) {
    ctsem::ctModel(
      type = "ct",
      n.manifest = 1L,
      n.latent = 1L,
      manifestNames = manifest_name,
      latentNames = "eta",
      LAMBDA = lam
    )
  } else {
    tp <- max(2L, as.integer(max_Tpoints)[1L])
    ctsem::ctModel(
      type = "omx",
      n.manifest = 1L,
      n.latent = 1L,
      Tpoints = tp,
      manifestNames = manifest_name,
      latentNames = "eta",
      LAMBDA = lam
    )
  }
}

#' @keywords internal
#' @noRd
.ild_ctsem_fit_dispatch <- function(dat, ct_model, model_type, dots = list()) {
  if (identical(model_type, "stanct")) {
    tries <- list(
      c(list(datalong = dat, ctstanmodel = ct_model), dots),
      c(list(data = dat, ctstanmodel = ct_model), dots)
    )
    for (args in tries) {
      fit <- tryCatch(do.call(ctsem::ctStanFit, args), error = function(e) e)
      if (!inherits(fit, "error")) {
        return(list(fit = fit, fit_type = "stanct", fit_call = "ctStanFit"))
      }
    }
    stop("ctStanFit failed for provided data/model; verify ct_model compatibility.", call. = FALSE)
  }

  tries <- list(
    c(list(data = dat, ctmodelobj = ct_model), dots),
    c(list(datalong = dat, ctmodelobj = ct_model), dots)
  )
  for (args in tries) {
    fit <- tryCatch(do.call(ctsem::ctFit, args), error = function(e) e)
    if (!inherits(fit, "error")) {
      return(list(fit = fit, fit_type = "ctfit", fit_call = "ctFit"))
    }
  }
  stop("ctFit failed for provided data/model; verify ct_model compatibility.", call. = FALSE)
}

#' @keywords internal
#' @noRd
.ild_ctsem_is_converged <- function(object) {
  fit <- if (inherits(object, "ild_fit_ctsem")) object$ct_fit else object
  if (is.null(fit)) return(NA)
  cv <- tryCatch(fit$optimization$convergence, error = function(e) NULL)
  if (is.null(cv)) cv <- tryCatch(fit$opt$convergence, error = function(e) NULL)
  if (is.null(cv)) cv <- tryCatch(fit$stanfit@sim$adaptation_info, error = function(e) NULL)
  if (is.numeric(cv) && length(cv) > 0L) return(isTRUE(cv[1L] == 0))
  if (is.logical(cv) && length(cv) > 0L) return(isTRUE(cv[1L]))
  NA
}
