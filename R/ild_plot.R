#' ILD-specific plots
#'
#' Produces trajectory (spaghetti), heatmap, gaps, and (if a fitted model
#' is provided) fitted vs actual and residual ACF. Works for both lmerMod and
#' lme (ild_lme with ar1 = TRUE).
#'
#' @param x An ILD tibble or a fitted [ild_lme()] model.
#' @param type Character (or vector). One or more of: `"trajectory"`, `"heatmap"`,
#'   `"gaps"`, `"missingness"`, `"fitted"` or `"fitted_vs_actual"` (requires fitted model),
#'   `"residual_acf"` (requires fitted model; ACF is over observation sequence, not adjusted for
#'   irregular time gaps). If length > 1, returns a named list of ggplots.
#' @param var For `trajectory` or `heatmap`, the variable to plot (optional;
#'   if missing and only one non-.ild_* column exists, it is used).
#' @param id_var For trajectory, variable used for grouping (default `.ild_id`).
#' @param time_var For trajectory/gaps, x-axis: `.ild_time_num` or `.ild_seq`.
#' @param max_ids For trajectory, max number of persons to plot (sampled if
#'   larger; default 20). Set to `Inf` to plot all.
#' @param seed Integer. Seed for sampling ids when `max_ids` is set (default 42).
#' @param ... Unused.
#' @return A single ggplot when `length(type) == 1`, or a named list of ggplots when `length(type) > 1`.
#' @examples
#' x <- ild_prepare(ild_simulate(n_id = 3, n_obs_per = 6, seed = 1), id = "id", time = "time")
#' fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
#' ild_plot(fit, type = "fitted_vs_actual")
#' ild_plot(fit, type = c("fitted_vs_actual", "residual_acf"))
#' @importFrom ggplot2 aes geom_line geom_point geom_tile geom_abline labs theme_minimal ggplot
#' @export
ild_plot <- function(x,
                     type = c("trajectory", "heatmap", "gaps", "missingness", "fitted", "fitted_vs_actual", "residual_acf"),
                     var = NULL,
                     id_var = ".ild_id",
                     time_var = c(".ild_time_num", ".ild_seq"),
                     max_ids = 20L,
                     seed = 42L,
                     ...) {
  type <- match.arg(type, several.ok = TRUE)
  time_var <- match.arg(time_var)
  fit_types <- c("fitted", "fitted_vs_actual", "residual_acf")
  if (any(type %in% fit_types)) {
    data <- if (inherits(x, "ild_lme") || !is.null(attr(x, "ild_data", exact = TRUE))) attr(x, "ild_data", exact = TRUE) else NULL
    if (is.null(data)) stop("Fitted model is missing ild_data attribute.", call. = FALSE)
  } else {
    data <- if (inherits(x, "ild_lme") || !is.null(attr(x, "ild_data", exact = TRUE))) attr(x, "ild_data", exact = TRUE) else x
  }
  if (!is.null(data)) validate_ild(data)
  ild_cols <- c(".ild_id", ".ild_time", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap")
  data_cols <- setdiff(names(data), ild_cols)
  if (any(type %in% c("trajectory", "heatmap")) && is.null(var)) {
    if (length(data_cols) == 0) stop("No data columns to plot.", call. = FALSE)
    if (length(data_cols) > 1) stop("Specify 'var' when data has multiple non-.ild_* columns.", call. = FALSE)
    var <- data_cols[1]
  }
  one_plot <- function(t) {
    if (t == "fitted_vs_actual") t <- "fitted"
    if (t == "missingness") {
      miss_vars <- if (is.null(var)) data_cols else var
      if (length(miss_vars) == 0) stop("No data columns for missingness plot.", call. = FALSE)
      return(ild_plot_missingness(data, miss_vars, id_var, time_var))
    }
    switch(t,
      trajectory = ild_plot_trajectory(data, var, id_var, time_var, max_ids, seed),
      heatmap = ild_plot_heatmap(data, var, id_var, time_var),
      gaps = ild_plot_gaps(data, id_var, time_var),
      fitted = ild_plot_fitted(x, data),
      residual_acf = ild_plot_residual_acf(x, data)
    )
  }
  if (length(type) == 1) {
    if (type == "missingness") {
      miss_vars <- if (is.null(var)) data_cols else var
      if (length(miss_vars) == 0) stop("No data columns for missingness plot.", call. = FALSE)
      return(ild_plot_missingness(data, miss_vars, id_var, time_var))
    }
    plot_type <- if (type == "fitted_vs_actual") "fitted" else type
    return(switch(plot_type,
      trajectory = ild_plot_trajectory(data, var, id_var, time_var, max_ids, seed),
      heatmap = ild_plot_heatmap(data, var, id_var, time_var),
      gaps = ild_plot_gaps(data, id_var, time_var),
      fitted = ild_plot_fitted(x, data),
      residual_acf = ild_plot_residual_acf(x, data)
    ))
  }
  out <- lapply(type, one_plot)
  names(out) <- type
  out
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
  aug <- augment_ild_model(fit)
  resp_name <- setdiff(names(aug), c(".ild_id", ".ild_time", ".fitted", ".resid"))[1L]
  if (is.na(resp_name)) resp_name <- "outcome"
  ggplot2::ggplot(aug, ggplot2::aes(x = .data[[resp_name]], y = .data$.fitted, color = .data$.ild_id)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::labs(x = "Observed", y = "Fitted", title = "Fitted vs Observed") +
    ggplot2::theme_minimal()
}

ild_plot_residual_acf <- function(fit, data) {
  diag <- ild_diagnostics(fit, data = data, by_id = FALSE, type = "residual_acf")
  acf_pooled <- diag$stats$acf$pooled
  if (is.null(acf_pooled) || nrow(acf_pooled) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() + ggplot2::labs(title = "ACF (insufficient data)"))
  }
  ggplot2::ggplot(acf_pooled, ggplot2::aes(x = .data$lag, y = .data$acf)) +
    ggplot2::geom_col(width = 0.1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Lag", y = "ACF", title = "Residual ACF") +
    ggplot2::theme_minimal()
}

#' ILD heatmap (alias for ild_plot with type = "heatmap")
#'
#' Person x time heatmap of a variable. See [ild_plot()].
#' @param x ILD object or fitted model (for heatmap, data are taken from ild_data if model).
#' @param var Variable to plot. If NULL, single data column is used.
#' @param ... Passed to [ild_plot()] (e.g. id_var, time_var).
#' @return A ggplot object.
#' @export
ild_heatmap <- function(x, var = NULL, ...) {
  ild_plot(x, type = "heatmap", var = var, ...)
}

#' ILD spaghetti / person trajectories (alias for ild_plot with type = "trajectory")
#'
#' Line plot of variable over time, one line per person. See [ild_plot()].
#' @param x ILD object or fitted model.
#' @param var Variable to plot. If NULL, single data column is used.
#' @param ... Passed to [ild_plot()] (e.g. max_ids, seed, id_var, time_var).
#' @return A ggplot object.
#' @export
ild_spaghetti <- function(x, var = NULL, ...) {
  ild_plot(x, type = "trajectory", var = var, ...)
}
