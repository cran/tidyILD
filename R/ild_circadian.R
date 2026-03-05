#' Time-of-day pattern plot for ILD (circadian-style)
#'
#' Plots a variable by hour of day (or time-of-day) when \code{.ild_time} is
#' POSIXct. Useful for EMA (e.g. mood or activity by hour). Does not add
#' columns to the ILD object; hour is derived internally for plotting.
#'
#' @param x An ILD object (see [is_ild()]) with \code{.ild_time} as POSIXct.
#' @param var Character or symbol. Variable to plot (e.g. mood, activity).
#' @param type Character. \code{"boxplot"} (default) or \code{"line"} (mean per hour with optional SE).
#' @return A ggplot object (variable by hour of day).
#' @export
#' @examples
#' d <- ild_simulate(n_id = 5, n_obs_per = 12, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' ild_circadian(x, y)
ild_circadian <- function(x, var, type = c("boxplot", "line")) {
  validate_ild(x)
  type <- match.arg(type)
  var <- as.character(rlang::ensym(var))
  if (!var %in% names(x)) stop("Variable '", var, "' not found in data.", call. = FALSE)
  tcol <- x[[".ild_time"]]
  if (!lubridate::is.POSIXt(tcol)) {
    stop(".ild_time must be POSIXct/POSIXlt for circadian plot. Use ild_plot(type = 'trajectory') for other time axes.",
         call. = FALSE)
  }
  hour <- as.integer(lubridate::hour(tcol))
  df <- data.frame(
    hour = hour,
    value = x[[var]],
    id = x[[".ild_id"]]
  )
  df <- df[!is.na(df$value), ]
  if (nrow(df) == 0) stop("No non-NA values for '", var, "'.", call. = FALSE)
  if (type == "boxplot") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(.data$hour), y = .data$value)) +
      ggplot2::geom_boxplot(fill = "gray80", outlier.alpha = 0.5) +
      ggplot2::labs(x = "Hour of day", y = var, title = paste0(var, " by hour of day")) +
      ggplot2::theme_minimal()
  } else {
    agg <- dplyr::summarise(
      dplyr::group_by(df, .data$hour),
      mean = mean(.data$value, na.rm = TRUE),
      se = stats::sd(.data$value, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )
    p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data$hour, y = .data$mean)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean - .data$se, ymax = .data$mean + .data$se), width = 0.2) +
      ggplot2::labs(x = "Hour of day", y = paste0("Mean ", var), title = paste0(var, " by hour of day")) +
      ggplot2::theme_minimal()
  }
  p
}
