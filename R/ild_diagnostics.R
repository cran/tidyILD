#' Residual diagnostics for an ILD model
#'
#' Computes residual ACF (by person and/or pooled), residual vs fitted,
#' residual vs time, and optional Q-Q. For `ild_lme` models with `ar1 = TRUE`,
#' reports the estimated AR/CAR parameter.
#'
#' @param object A fitted model from [ild_lme()] (or an object with
#'   `residuals()`, and optional `fitted()`; if not `ild_lme`, pass `data` with
#'   `.ild_id` and `.ild_time_num` or `.ild_seq`).
#' @param data Optional. ILD data (required if `object` is not from [ild_lme()]).
#' @param by_id Logical. If `TRUE`, compute ACF within each person (default `TRUE`).
#' @param ... Unused.
#' @return A list with: `acf` (ACF values or list by id), `residuals`, `fitted`,
#'   `id`, `time` (or `seq`), `ar1_param` (if applicable), and `plot` (a ggplot or list of plots).
#' @importFrom ggplot2 aes geom_point geom_hline labs theme_minimal ggplot stat_qq stat_qq_line
#' @importFrom stats coef
#' @export
ild_diagnostics <- function(object, data = NULL, by_id = TRUE, ...) {
  if (inherits(object, "ild_lme") || !is.null(attr(object, "ild_data", exact = TRUE))) {
    data <- attr(object, "ild_data", exact = TRUE)
    if (is.null(data)) stop("Model object missing ild_data attribute.", call. = FALSE)
  }
  if (is.null(data)) stop("Provide 'data' (ILD object) for diagnostics.", call. = FALSE)
  validate_ild(data)
  id_col <- ".ild_id"
  time_num <- data[[".ild_time_num"]]
  seq_col <- ".ild_seq"
  res <- stats::residuals(object)
  if (length(res) != nrow(data)) {
    stop("Residual length does not match data. Use the same data as in the fit.", call. = FALSE)
  }
  fitted_vals <- tryCatch(stats::fitted(object), error = function(e) NULL)
  ar1_param <- NULL
  if (inherits(object, "ild_lme") && isTRUE(attr(object, "ild_ar1"))) {
    if (inherits(object, "lme")) {
      cs <- object$modelStruct$corStruct
      if (!is.null(cs)) {
        phi <- tryCatch(coef(cs, unconstrained = FALSE)[[1]], error = function(e) NULL)
        if (!is.null(phi)) ar1_param <- phi
      }
    }
  }
  out <- list(
    residuals = res,
    fitted = fitted_vals,
    id = data[[id_col]],
    time = time_num,
    seq = data[[seq_col]],
    ar1_param = ar1_param,
    by_id = by_id
  )
  if (by_id) {
    ids <- unique(data[[id_col]])
    acf_by_id <- vector("list", length(ids))
    names(acf_by_id) <- as.character(ids)
    for (i in seq_along(ids)) {
      idx <- data[[id_col]] == ids[i]
      r <- res[idx]
      r <- r[!is.na(r)]
      if (length(r) > 2) {
        acf_by_id[[i]] <- stats::acf(r, plot = FALSE, na.action = stats::na.pass)
      } else {
        acf_by_id[[i]] <- NULL
      }
    }
    out$acf <- acf_by_id
  } else {
    r <- res[!is.na(res)]
    out$acf <- if (length(r) > 2) stats::acf(r, plot = FALSE, na.action = stats::na.pass) else NULL
  }
  out$plot <- plot_ild_diagnostics(out)
  class(out) <- "ild_diagnostics"
  out
}

#' @noRd
plot_ild_diagnostics <- function(diag) {
  p_res_fitted <- NULL
  if (!is.null(diag$fitted) && length(diag$fitted) == length(diag$residuals)) {
    df <- data.frame(
      fitted = diag$fitted,
      residuals = diag$residuals,
      id = diag$id
    )
    p_res_fitted <- ggplot2::ggplot(df, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$id), alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") +
      ggplot2::theme_minimal()
  }
  p_res_time <- NULL
  if (!is.null(diag$time)) {
    df <- data.frame(residuals = diag$residuals, time = diag$time, id = diag$id)
    p_res_time <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$residuals)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$id), alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(title = "Residuals vs Time", x = "Time", y = "Residuals") +
      ggplot2::theme_minimal()
  }
  p_qq <- NULL
  r <- diag$residuals[!is.na(diag$residuals)]
  if (length(r) > 0) {
    df_qq <- data.frame(residuals = r)
    p_qq <- ggplot2::ggplot(df_qq, ggplot2::aes(sample = .data$residuals)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::labs(title = "Q-Q plot (residuals)", x = "Theoretical", y = "Sample") +
      ggplot2::theme_minimal()
  }
  list(
    residuals_vs_fitted = p_res_fitted,
    residuals_vs_time = p_res_time,
    qq = p_qq
  )
}
