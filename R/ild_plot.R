#' ILD-specific plots
#'
#' Produces trajectory (spaghetti), heatmap, gaps, and (if a fitted model
#' is provided) fitted vs observed and residual ACF.
#'
#' @param x An ILD tibble or a fitted [ild_lme()] model.
#' @param type Character. One of `"trajectory"`, `"heatmap"`, `"gaps"`,
#'   `"missingness"` (person x time missingness), `"fitted"` (requires fitted model),
#'   `"residual_acf"` (requires fitted model).
#' @param var For `trajectory` or `heatmap`, the variable to plot (optional;
#'   if missing and only one non-.ild_* column exists, it is used).
#' @param id_var For trajectory, variable used for grouping (default `.ild_id`).
#' @param time_var For trajectory/gaps, x-axis: `.ild_time_num` or `.ild_seq`.
#' @param max_ids For trajectory, max number of persons to plot (sampled if
#'   larger; default 20). Set to `Inf` to plot all.
#' @param seed Integer. Seed for sampling ids when `max_ids` is set (default 42).
#' @param ... Unused.
#' @return A ggplot object, or a list of plots for diagnostics.
#' @importFrom ggplot2 aes geom_line geom_point geom_tile geom_abline labs theme_minimal ggplot
#' @export
ild_plot <- function(x,
                     type = c("trajectory", "heatmap", "gaps", "missingness", "fitted", "residual_acf"),
                     var = NULL,
                     id_var = ".ild_id",
                     time_var = c(".ild_time_num", ".ild_seq"),
                     max_ids = 20L,
                     seed = 42L,
                     ...) {
  type <- match.arg(type)
  time_var <- match.arg(time_var)
  data <- if (inherits(x, "ild_lme") || !is.null(attr(x, "ild_data", exact = TRUE))) attr(x, "ild_data", exact = TRUE) else x
  if (is.null(data) && type %in% c("fitted", "residual_acf")) {
    stop("Fitted model is missing ild_data attribute.", call. = FALSE)
  }
  if (!is.null(data)) validate_ild(data)
  ild_cols <- c(".ild_id", ".ild_time", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap")
  data_cols <- setdiff(names(data), ild_cols)
  if (type %in% c("trajectory", "heatmap") && is.null(var)) {
    if (length(data_cols) == 0) stop("No data columns to plot.", call. = FALSE)
    if (length(data_cols) > 1) stop("Specify 'var' when data has multiple non-.ild_* columns.", call. = FALSE)
    var <- data_cols[1]
  }
  if (type == "missingness") {
    miss_vars <- if (is.null(var)) data_cols else var
    if (length(miss_vars) == 0) stop("No data columns for missingness plot.", call. = FALSE)
    return(ild_plot_missingness(data, miss_vars, id_var, time_var))
  }
  switch(type,
    trajectory = ild_plot_trajectory(data, var, id_var, time_var, max_ids, seed),
    heatmap = ild_plot_heatmap(data, var, id_var, time_var),
    gaps = ild_plot_gaps(data, id_var, time_var),
    fitted = ild_plot_fitted(x, data),
    residual_acf = ild_plot_residual_acf(x, data)
  )
}

ild_plot_trajectory <- function(data, var, id_var, time_var, max_ids, seed) {
  ids <- unique(data[[id_var]])
  if (length(ids) > max_ids && is.finite(max_ids)) {
    set.seed(seed)
    ids <- sample(ids, max_ids)
    data <- data[data[[id_var]] %in% ids, ]
  }
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[time_var]], y = .data[[var]], group = .data[[id_var]])) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::geom_point(alpha = 0.5, size = 1) +
    ggplot2::labs(x = "Time", y = var, title = "Trajectories") +
    ggplot2::theme_minimal()
}

ild_plot_heatmap <- function(data, var, id_var, time_var) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[time_var]], y = factor(.data[[id_var]]), fill = .data[[var]])) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Time", y = "Person", fill = var, title = "Heatmap") +
    ggplot2::theme_minimal()
}

ild_plot_gaps <- function(data, id_var, time_var) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[time_var]], y = .data[[".ild_dt"]], color = .data[[id_var]])) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(x = "Time", y = "Interval", title = "Intervals (gaps)") +
    ggplot2::theme_minimal()
}

ild_plot_missingness <- function(data, vars, id_var, time_var) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) stop("No matching columns for missingness plot.", call. = FALSE)
  parts <- lapply(vars, function(v) {
    data.frame(
      id = data[[id_var]],
      time = data[[time_var]],
      variable = v,
      missing = is.na(data[[v]]),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, parts)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time, y = factor(.data$id), fill = .data$missing)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c("FALSE" = "gray90", "TRUE" = "darkred"), labels = c("observed", "missing")) +
    ggplot2::labs(x = "Time", y = "Person", fill = NULL, title = "Missingness") +
    ggplot2::theme_minimal()
  if (length(vars) > 1) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$variable), ncol = 1)
  p
}

ild_plot_fitted <- function(fit, data) {
  f <- stats::fitted(fit)
  mf <- tryCatch(stats::model.frame(fit, data = data), error = function(e) stats::model.frame(fit))
  y <- stats::model.response(mf)
  id_col <- ".ild_id"
  id_vals <- if (id_col %in% names(data)) data[[id_col]] else seq_along(f)
  df <- data.frame(fitted = f, observed = y, id = id_vals)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$observed, y = .data$fitted, color = .data$id)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::labs(x = "Observed", y = "Fitted", title = "Fitted vs Observed") +
    ggplot2::theme_minimal()
}

ild_plot_residual_acf <- function(fit, data) {
  diag <- ild_diagnostics(fit, data = data, by_id = FALSE)
  acf_obj <- diag$acf
  if (is.null(acf_obj)) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() + ggplot2::labs(title = "ACF (insufficient data)"))
  }
  lag <- acf_obj$lag
  acf_vals <- as.vector(acf_obj$acf)
  df <- data.frame(lag = lag, acf = acf_vals)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$lag, y = .data$acf)) +
    ggplot2::geom_col(width = 0.1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Lag", y = "ACF", title = "Residual ACF") +
    ggplot2::theme_minimal()
}
