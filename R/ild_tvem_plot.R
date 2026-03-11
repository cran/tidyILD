#' Plot the time-varying coefficient from a TVEM fit
#'
#' Builds a line plot of the smooth term for the time-varying effect of the
#' predictor (with optional confidence band). Uses a grid over the time variable
#' and [mgcv::predict.gam()] with \code{type = "terms"}.
#'
#' @param tvem_fit A fitted object from [ild_tvem()] (class \code{tidyild_tvem}).
#' @param n_grid Integer. Number of points over the time range for the curve (default 100).
#' @param level Numeric. Confidence level for the band (default 0.95).
#' @return A \code{ggplot} object (time on x-axis, estimated effect on y-axis).
#' @export
#' @examples
#' set.seed(1)
#' d <- ild_simulate(n_id = 10, n_obs_per = 15, seed = 1)
#' d$x <- rnorm(nrow(d))
#' x <- ild_prepare(d, id = "id", time = "time")
#' tv <- ild_tvem(x, "y", "x", k = 5, re_id = TRUE)
#' ild_tvem_plot(tv)
ild_tvem_plot <- function(tvem_fit, n_grid = 100L, level = 0.95) {
  if (!inherits(tvem_fit, "tidyild_tvem")) {
    stop("tvem_fit must be from ild_tvem().", call. = FALSE)
  }
  meta <- attr(tvem_fit, "ild_tvem_meta", exact = TRUE)
  if (is.null(meta)) stop("tvem_fit missing ild_tvem_meta.", call. = FALSE)
  time_var <- meta$time_var
  predictor <- meta$predictor
  outcome <- meta$outcome
  dat <- tvem_fit$model
  if (is.null(dat) || !time_var %in% names(dat)) {
    stop("Model frame missing or missing time_var.", call. = FALSE)
  }
  t_range <- range(dat[[time_var]], na.rm = TRUE)
  t_grid <- seq(t_range[1], t_range[2], length.out = n_grid)
  newdata <- data.frame(t_grid, 1)
  names(newdata) <- c(time_var, predictor)
  if (".ild_id" %in% names(dat) && meta$re_id) {
    id_levs <- levels(factor(dat[[".ild_id"]]))
    newdata$.ild_id <- factor(id_levs[1L], levels = id_levs)
  }
  pred <- mgcv::predict.gam(tvem_fit, newdata = newdata, type = "terms", se.fit = TRUE)
  term_names <- colnames(pred$fit)
  idx <- which(grepl(predictor, term_names, fixed = TRUE) | grepl("by", term_names, fixed = TRUE))
  if (length(idx) == 0L) idx <- 2L
  if (length(idx) > 1L) idx <- idx[1L]
  fit_vals <- as.vector(pred$fit[, idx])
  se_vals <- as.vector(pred$se.fit[, idx])
  half <- (1 - level) / 2
  z <- stats::qnorm(1 - half)
  df_plot <- data.frame(
    time = t_grid,
    effect = fit_vals,
    se = se_vals,
    ci_low = fit_vals - z * se_vals,
    ci_high = fit_vals + z * se_vals
  )
  ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$time, y = .data$effect)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high), alpha = 0.2, fill = "gray50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      x = time_var,
      y = paste0("Time-varying effect (", predictor, ")"),
      title = paste0("TVEM: ", outcome, " ~ s(", time_var, ") + s(", time_var, ", by = ", predictor, ")")
    ) +
    ggplot2::theme_minimal()
}
