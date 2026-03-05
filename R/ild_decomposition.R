#' Within-person and between-person variance decomposition
#'
#' Reports WP and BP variance and their ratio for selected variables. Use as a
#' diagnostic and teaching tool: large ratio suggests within-person variance
#' dominates; small ratio suggests between-person differences dominate. Helps
#' avoid conflating WP and BP effects in modeling.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param ... Variables to decompose (tidy-select). Must be numeric.
#' @param plot Logical. If \code{TRUE}, return a list with \code{table} and \code{plot} (WP vs BP density overlay). Default \code{FALSE}.
#' @return A tibble with columns \code{variable}, \code{wp_var}, \code{bp_var}, \code{ratio} (wp_var / bp_var; \code{Inf} if bp_var is 0). If \code{plot = TRUE}, a list with \code{table} and \code{plot} (ggplot).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' ild_decomposition(x, y)
#' @importFrom dplyr select
ild_decomposition <- function(x, ..., plot = FALSE) {
  validate_ild(x)
  id_col <- ".ild_id"
  vars <- names(dplyr::select(x, ...))
  if (length(vars) == 0) stop("No variables selected for decomposition.", call. = FALSE)
  for (v in vars) {
    if (!v %in% names(x)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    if (!is.numeric(x[[v]])) stop("Variable '", v, "' is not numeric.", call. = FALSE)
  }
  out_rows <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    v <- vars[i]
    means_i <- tapply(x[[v]], x[[id_col]], mean, na.rm = TRUE)
    bp_var <- stats::var(as.vector(means_i))
    if (is.na(bp_var)) bp_var <- 0
    wp_var_by_id <- tapply(x[[v]], x[[id_col]], function(z) stats::var(z, na.rm = TRUE))
    wp_var_by_id[is.na(wp_var_by_id)] <- 0
    wp_var <- mean(wp_var_by_id, na.rm = TRUE)
    if (is.na(wp_var)) wp_var <- 0
    ratio <- if (bp_var > 0) wp_var / bp_var else Inf
    out_rows[[i]] <- list(variable = v, wp_var = wp_var, bp_var = bp_var, ratio = ratio)
  }
  tbl <- tibble::tibble(
    variable = vapply(out_rows, `[[`, character(1), "variable"),
    wp_var = vapply(out_rows, `[[`, double(1), "wp_var"),
    bp_var = vapply(out_rows, `[[`, double(1), "bp_var"),
    ratio = vapply(out_rows, `[[`, double(1), "ratio")
  )
  if (!plot) return(tbl)
  p <- plot_ild_decomposition(x, vars)
  list(table = tbl, plot = p)
}

#' Plot WP vs BP density for decomposition (internal / called by ild_decomposition when plot=TRUE)
#' @param x ILD object
#' @param vars character vector of variable names
#' @return ggplot object (overlay density of WP and BP components for first variable)
#' @noRd
plot_ild_decomposition <- function(x, vars) {
  id_col <- ".ild_id"
  v <- vars[1]
  means_i <- tapply(x[[v]], x[[id_col]], mean, na.rm = TRUE)
  person_means <- means_i[as.character(x[[id_col]])]
  wp_vals <- x[[v]] - person_means
  bp_vals <- as.vector(means_i)
  df_wp <- data.frame(value = wp_vals, component = "WP (within-person deviation)")
  df_bp <- data.frame(value = bp_vals, component = "BP (person mean)")
  df <- rbind(df_wp, df_bp)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$value, color = .data$component, fill = .data$component)) +
    ggplot2::geom_density(alpha = 0.3) +
    ggplot2::labs(title = paste0("WP vs BP: ", v), x = v, y = "Density") +
    ggplot2::theme_minimal()
}

#' Standalone WP/BP centering plot
#'
#' Shows within-person deviation and between-person (person mean) distribution
#' for selected variable(s). Uses the same plot as [ild_decomposition(..., plot = TRUE)].
#' Useful when you only want the visualization without the variance table.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param ... Variables to plot (tidy-select). Must be numeric. Only the first is plotted.
#' @return A ggplot object (WP vs BP density overlay for the first selected variable).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' ild_center_plot(x, y)
ild_center_plot <- function(x, ...) {
  validate_ild(x)
  vars <- names(dplyr::select(x, ...))
  if (length(vars) == 0) stop("No variables selected for center plot.", call. = FALSE)
  for (v in vars) {
    if (!v %in% names(x)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    if (!is.numeric(x[[v]])) stop("Variable '", v, "' is not numeric.", call. = FALSE)
  }
  plot_ild_decomposition(x, vars)
}
