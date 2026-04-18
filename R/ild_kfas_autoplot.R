# Autoplot for ild_fit_kfas

#' @rdname ild_autoplot
#' @param type Character: \code{"states"} (smoothed level vs time), \code{"innovations"}
#'   (standardized), or \code{"signal"} (observation expectation vs outcome).
#' @method ild_autoplot ild_fit_kfas
#' @export
ild_autoplot.ild_fit_kfas <- function(x, type = c("states", "innovations", "signal"), ...) {
  rlang::check_installed("KFAS", reason = "to plot ild_kfas fits")
  type <- match.arg(type)
  dat <- attr(x, "ild_data", exact = TRUE)
  if (is.null(dat)) {
    stop("ild_autoplot() for ild_fit_kfas requires attr(fit, \"ild_data\").", call. = FALSE)
  }
  resp <- attr(x, "ild_response", exact = TRUE)
  y <- if (!is.null(resp)) dat[[resp]] else NA
  kfs <- x$kfs
  n <- nrow(dat)
  tidx <- seq_len(n)

  if (type == "states") {
    return(ild_plot_states(x, state_index = 1L, ...))
  }

  if (type == "innovations") {
    r <- tryCatch(stats::rstandard(kfs), error = function(e) NULL)
    if (is.null(r)) {
      stop("Could not extract standardized innovations from KFS output.", call. = FALSE)
    }
    df <- data.frame(time = tidx, r = as.numeric(r)[seq_len(n)], stringsAsFactors = FALSE)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$r)) +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(
          title = "Standardized one-step-ahead innovations",
          x = "Time index",
          y = "rstandard(KFS)"
        ) +
        ggplot2::theme_minimal()
    )
  }

  # signal
  ex <- ild_kfas_extract_augment(kfs, y, TRUE)
  df <- data.frame(
    time = tidx,
    y = as.numeric(y),
    fitted = ex$fitted,
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$y, colour = "y")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted, colour = "fitted")) +
    ggplot2::labs(
      title = "Outcome vs signal expectation",
      x = "Time index",
      y = NULL,
      colour = NULL
    ) +
    ggplot2::theme_minimal()
}
