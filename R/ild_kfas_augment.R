# Augment method for ild_kfas

#' @rdname ild_augment
#' @method ild_augment ild_fit_kfas
#' @export
ild_augment.ild_fit_kfas <- function(x, ...) {
  rlang::check_installed("KFAS", reason = "to use ild_augment() for ild_kfas fits")
  data <- attr(x, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop("ild_augment() for ild_fit_kfas requires attr(fit, \"ild_data\").", call. = FALSE)
  }
  validate_ild(data)
  resp <- attr(x, "ild_response", exact = TRUE)
  if (is.null(resp)) {
    stop("ild_response attribute missing; refit with ild_kfas().", call. = FALSE)
  }
  y <- data[[resp]]
  y <- if (is.numeric(y)) y else suppressWarnings(as.numeric(y))

  ex <- ild_kfas_extract_augment(x$kfs, y, use_smoother = TRUE)
  fit <- ex$fitted
  resid <- ex$resid
  rs <- ex$resid_std
  rs_k <- tryCatch(
    as.numeric(stats::rstandard(x$kfs)),
    error = function(e) NULL
  )
  if (!is.null(rs_k) && length(rs_k) == length(y)) {
    rs <- rs_k
  }

  st <- ild_kfas_extract_level_state(x$kfs)
  opt <- list()
  if (!is.null(st$mean) && length(st$mean) == nrow(data)) {
    opt$.state <- st$mean
    # crude 95% for smoothed state if V available
    V <- x$kfs$V
    if (!is.null(V) && length(dim(V)) >= 3) {
      lo <- hi <- rep(NA_real_, nrow(data))
      for (i in seq_len(nrow(data))) {
        v <- tryCatch(V[1L, 1L, i], error = function(e) NA_real_)
        if (is.finite(v) && v >= 0) {
          se <- sqrt(v)
          lo[i] <- st$mean[i] - 1.96 * se
          hi[i] <- st$mean[i] + 1.96 * se
        }
      }
      opt$.state_lower <- lo
      opt$.state_upper <- hi
    }
  }

  ild_augment_assemble(
    .ild_id = data[[".ild_id"]],
    .ild_time = data[[".ild_time"]],
    .outcome = y,
    .fitted = fit,
    .resid = resid,
    .resid_std = rs,
    engine = "KFAS",
    model_class = "ild_fit_kfas",
    optional = opt
  )
}

#' Per-row latent states (long format) for complex multi-state models
#'
#' For \code{local_level} only, use [ild_augment()] which already provides
#' \code{.state} for the level. This function is reserved for future multi-state
#' models.
#'
#' @param x An object from [ild_kfas()].
#' @param ... Reserved.
#' @return A \code{\link[tibble]{tibble}}.
#' @export
ild_augment_states <- function(x, ...) {
  UseMethod("ild_augment_states")
}

#' @export
ild_augment_states.ild_fit_kfas <- function(x, ...) {
  ild_tidy_states(x)
}
