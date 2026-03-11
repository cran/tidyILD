#' Residual diagnostics for an ILD model
#'
#' Computes residual ACF (by person and/or pooled), residual vs fitted,
#' residual vs time, and optional Q-Q. Use `type` to request only specific
#' diagnostics. For `ild_lme` models with `ar1 = TRUE`, the estimated AR/CAR
#' parameter is reported when `type` includes `"residual_acf"`.
#'
#' The return value follows a stable schema: `meta` (engine, ar1, id/time columns,
#' n_obs, n_id), `data$residuals` (tibble with `.ild_id`, `.ild_time`, response, `.resid`, `.fitted`),
#' and `stats` (e.g. `acf`, `ar1_param`). Plots are not stored in the object; use
#' [plot_ild_diagnostics()] to generate them from a diagnostics object. The column
#' \code{.resid} is always filled; \code{.fitted} is filled when it can be computed
#' without refitting, otherwise it is \code{NA} (same for both engines and all \code{type} values).
#'
#' Residual ACF is computed over the ordered observation sequence within person;
#' it does not adjust for irregular time gaps.
#'
#' @param object A fitted model from [ild_lme()] (or an object with
#'   `residuals()`, and optional `fitted()`; if not `ild_lme`, pass `data` with
#'   `.ild_id` and `.ild_time_num` or `.ild_seq`).
#' @param data Optional. ILD data (required if `object` is not from [ild_lme()]).
#' @param type Character vector. Which diagnostics to compute: `"residual_acf"`,
#'   `"residual_time"` (residuals vs time and vs fitted), `"qq"`. Default is all three.
#' @param by_id Logical. If `TRUE`, compute ACF within each person (default `TRUE`).
#' @param ... Unused.
#' @return A list of class `ild_diagnostics` with: `meta` (engine, ar1, id_col, time_col,
#'   n_obs, n_id, type, by_id), `data` (list with `residuals` = tibble of .ild_id, .ild_time,
#'   response (name from formula), .resid, .fitted; \code{data$residuals} always exists, \code{.resid} is always filled,
#'   \code{.fitted} is returned when it can be computed without refitting, otherwise NA),
#'   `stats` (list with `acf` = list(pooled = tibble, by_id = list) when requested,
#'   `ar1_param` = numeric or NULL for lme). Use [plot_ild_diagnostics()] for plots.
#' @examples
#' x <- ild_prepare(ild_simulate(n_id = 3, n_obs_per = 6, seed = 1), id = "id", time = "time")
#' fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
#' diag <- ild_diagnostics(fit, type = c("residual_acf", "qq"))
#' plot_ild_diagnostics(diag)
#' @importFrom ggplot2 aes geom_point geom_hline geom_col labs theme_minimal ggplot stat_qq stat_qq_line
#' @importFrom stats coef
#' @export
ild_diagnostics <- function(object, data = NULL, type = c("residual_acf", "residual_time", "qq"), by_id = TRUE, ...) {
  type <- match.arg(type, choices = c("residual_acf", "residual_time", "qq"), several.ok = TRUE)
  if (inherits(object, "ild_lme") || !is.null(attr(object, "ild_data", exact = TRUE))) {
    data <- attr(object, "ild_data", exact = TRUE)
    if (is.null(data)) stop("Model object missing ild_data attribute.", call. = FALSE)
  }
  if (is.null(data)) stop("Provide 'data' (ILD object) for diagnostics.", call. = FALSE)
  validate_ild(data)
  meta_ild <- ild_meta(data)
  id_col <- ".ild_id"
  if (!is.null(attr(object, "ild_data", exact = TRUE))) {
    data_resid <- augment_ild_model(object)
  } else {
    res <- stats::residuals(object)
    if (length(res) != nrow(data)) stop("Residual length does not match data.", call. = FALSE)
    f <- tryCatch(stats::fitted(object), error = function(e) NULL)
    if (is.null(f) || length(f) != nrow(data)) f <- rep(NA_real_, nrow(data))
    mf <- tryCatch(stats::model.frame(object, data = data), error = function(e) stats::model.frame(object))
    y <- tryCatch(stats::model.response(mf), error = function(e) rep(NA_real_, nrow(data)))
    out_name <- ild_response_name(object)
    data_resid <- tibble::tibble(
      .ild_id = data[[id_col]],
      .ild_time = data[[".ild_time"]],
      .fitted = f,
      .resid = res
    )
    data_resid[[out_name]] <- y
    data_resid <- data_resid[c(".ild_id", ".ild_time", out_name, ".fitted", ".resid")]
  }
  res <- data_resid$.resid
  engine <- if (inherits(object, "lme")) "lme" else "lmer"
  ar1_flag <- isTRUE(attr(object, "ild_ar1", exact = TRUE))
  n_obs <- nrow(data)
  n_id <- length(unique(data[[id_col]]))
  meta <- list(
    engine = engine,
    ar1 = ar1_flag,
    id_col = meta_ild$ild_id,
    time_col = meta_ild$ild_time,
    n_obs = n_obs,
    n_id = n_id,
    type = type,
    by_id = by_id
  )
  stats <- list(acf = NULL, ar1_param = NULL)
  ar1_param <- NULL
  if ("residual_acf" %in% type && inherits(object, "ild_lme") && ar1_flag && inherits(object, "lme")) {
    cs <- object$modelStruct$corStruct
    if (!is.null(cs)) {
      phi <- tryCatch(coef(cs, unconstrained = FALSE)[[1]], error = function(e) NULL)
      if (!is.null(phi)) ar1_param <- phi
    }
  }
  stats$ar1_param <- ar1_param
  if ("residual_acf" %in% type) {
    r_pooled <- res[!is.na(res)]
    pooled_tbl <- if (length(r_pooled) > 2) {
      acf_obj <- stats::acf(r_pooled, plot = FALSE, na.action = stats::na.pass)
      tibble::tibble(lag = as.vector(acf_obj$lag), acf = as.vector(acf_obj$acf))
    } else {
      tibble::tibble(lag = numeric(), acf = numeric())
    }
    by_id_tbls <- NULL
    if (by_id) {
      ids <- unique(data[[id_col]])
      by_id_tbls <- vector("list", length(ids))
      names(by_id_tbls) <- as.character(ids)
      for (i in seq_along(ids)) {
        idx <- data[[id_col]] == ids[i]
        r <- res[idx]
        r <- r[!is.na(r)]
        if (length(r) > 2) {
          acf_i <- stats::acf(r, plot = FALSE, na.action = stats::na.pass)
          by_id_tbls[[i]] <- tibble::tibble(lag = as.vector(acf_i$lag), acf = as.vector(acf_i$acf))
        } else {
          by_id_tbls[[i]] <- tibble::tibble(lag = numeric(), acf = numeric())
        }
      }
    }
    stats$acf <- list(pooled = pooled_tbl, by_id = by_id_tbls)
  }
  out <- list(
    meta = meta,
    data = list(residuals = data_resid),
    stats = stats
  )
  class(out) <- "ild_diagnostics"
  attr(out, "ild_provenance") <- ild_new_analysis_provenance(object, "ild_diagnostics", list(
    type = type,
    by_id = by_id,
    engine = meta$engine
  ), list(n_obs = n_obs, n_residuals = nrow(data_resid)))
  out
}

#' Plot diagnostics from an ild_diagnostics object
#'
#' Generates ggplot objects for the requested diagnostic types. Plots are not
#' stored in the diagnostics object; call this function to create them.
#'
#' @param diag An object returned by [ild_diagnostics()].
#' @param type Character vector. Which plots to build (default: the types stored in \code{diag$meta$type}).
#' @return A named list of ggplot objects (e.g. \code{residual_acf}, \code{residuals_vs_fitted}, \code{residuals_vs_time}, \code{qq}).
#' @export
plot_ild_diagnostics <- function(diag, type = NULL) {
  if (!inherits(diag, "ild_diagnostics")) stop("diag must be an object from ild_diagnostics().", call. = FALSE)
  if (is.null(type)) type <- diag$meta$type
  type <- match.arg(type, choices = c("residual_acf", "residual_time", "qq"), several.ok = TRUE)
  dr <- diag$data$residuals
  out_plots <- list()
  if ("residual_acf" %in% type) {
    acf_pooled <- diag$stats$acf$pooled
    if (!is.null(acf_pooled) && nrow(acf_pooled) > 0) {
      p_acf <- ggplot2::ggplot(acf_pooled, ggplot2::aes(x = .data$lag, y = .data$acf)) +
        ggplot2::geom_col(width = 0.1) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(x = "Lag", y = "ACF", title = "Residual ACF") +
        ggplot2::theme_minimal()
      out_plots$residual_acf <- p_acf
    } else {
      out_plots$residual_acf <- ggplot2::ggplot() + ggplot2::theme_minimal() +
        ggplot2::labs(title = "Residual ACF (insufficient data)")
    }
  }
  if ("residual_time" %in% type) {
    has_fitted <- !all(is.na(dr$.fitted))
    if (has_fitted) {
      p_res_fitted <- ggplot2::ggplot(dr, ggplot2::aes(x = .data$.fitted, y = .data$.resid)) +
        ggplot2::geom_point(ggplot2::aes(color = .data$.ild_id), alpha = 0.7) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") +
        ggplot2::theme_minimal()
      out_plots$residuals_vs_fitted <- p_res_fitted
    }
    p_res_time <- ggplot2::ggplot(dr, ggplot2::aes(x = .data$.ild_time, y = .data$.resid)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$.ild_id), alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(title = "Residuals vs Time", x = "Time", y = "Residuals") +
      ggplot2::theme_minimal()
    out_plots$residuals_vs_time <- p_res_time
  }
  if ("qq" %in% type) {
    r <- dr$.resid[!is.na(dr$.resid)]
    if (length(r) > 0) {
      df_qq <- data.frame(residuals = r)
      p_qq <- ggplot2::ggplot(df_qq, ggplot2::aes(sample = .data$residuals)) +
        ggplot2::stat_qq() +
        ggplot2::stat_qq_line() +
        ggplot2::labs(title = "Q-Q plot (residuals)", x = "Theoretical", y = "Sample") +
        ggplot2::theme_minimal()
      out_plots$qq <- p_qq
    }
  }
  out_plots
}

#' @export
print.ild_diagnostics <- function(x, ...) {
  m <- x$meta
  cat("ILD diagnostics\n")
  cat("  Engine: ", m$engine, "\n", sep = "")
  cat("  AR1/CAR1: ", if (m$ar1) "yes" else "no", "\n", sep = "")
  cat("  Id column: ", m$id_col, "  Time column: ", m$time_col, "\n", sep = "")
  cat("  N persons: ", m$n_id, "  N observations: ", m$n_obs, "\n", sep = "")
  cat("  Types computed: ", paste(m$type, collapse = ", "), "\n", sep = "")
  if (!is.null(x$stats$ar1_param) && is.numeric(x$stats$ar1_param)) {
    cat("  Estimated correlation (phi): ", round(x$stats$ar1_param, 4), "\n", sep = "")
  }
  invisible(x)
}
