#' Spacing-aware lag within person
#'
#' Computes lagged values within each person. Use this instead of
#' [dplyr::lag()], which assumes equal spacing and no gaps and is unsafe
#' for irregular ILD.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param ... Variables to lag (tidy-select). Unquoted names or selection.
#' @param n Integer. Lag order (default 1 = previous observation).
#' @param mode Character. \code{"index"}: row-based lag. \code{"gap_aware"}: same but NA when interval exceeds \code{max_gap}. \code{"time_window"}: value from (time - window, time] with \code{resolution}.
#' @param max_gap Numeric. For \code{gap_aware} only. Same units as \code{.ild_time_num}.
#'   If \code{NULL}, uses \code{ild_meta(x)$ild_gap_threshold} (metadata-driven default).
#' @param window Numeric or lubridate duration. For \code{time_window} only: time window width.
#'   Numeric is in same units as \code{.ild_time_num} (e.g. seconds for POSIXct). You can pass
#'   a lubridate period/duration (e.g. \code{lubridate::hours(2)}); it is converted to seconds for POSIXct.
#' @param resolution Character. For \code{time_window}: \code{"closest_prior"} (default: most recent observation in window),
#'   \code{"last_in_window"}, or \code{"mean_in_window"}.
#' @return The same ILD tibble with new lag columns. ILD attributes preserved.
#' @importFrom dplyr group_by lag mutate select ungroup
#' @export
ild_lag <- function(x, ..., n = 1L, mode = c("index", "gap_aware", "time_window"),
                   max_gap = NULL, window = NULL,
                   resolution = c("closest_prior", "last_in_window", "mean_in_window")) {
  validate_ild(x)
  mode <- match.arg(mode)
  resolution <- match.arg(resolution)
  if (mode == "gap_aware" && is.null(max_gap)) {
    max_gap <- ild_meta(x)$ild_gap_threshold
    if (!is.finite(max_gap)) max_gap <- Inf
  }
  if (mode != "gap_aware" && is.null(max_gap)) max_gap <- Inf
  n <- as.integer(n)[1]
  if (n < 1 && mode != "time_window") stop("'n' must be >= 1 for index/gap_aware.", call. = FALSE)
  vars <- names(dplyr::select(x, ...))
  if (length(vars) == 0) stop("No variables selected for lag.", call. = FALSE)
  if (mode == "time_window") {
    if (is.null(window)) stop("'window' is required for mode = 'time_window'.", call. = FALSE)
    window <- ild_window_to_num(window, x)
    if (!is.finite(window) || window <= 0) stop("'window' must be a positive number for mode = 'time_window'.", call. = FALSE)
  }
  id_col <- ".ild_id"
  seq_col <- ".ild_seq"
  dt_col <- ".ild_dt"
  time_num_col <- ".ild_time_num"
  out <- x
  for (v in vars) {
    if (!v %in% names(out)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    lag_nm <- if (mode == "time_window") paste0(v, "_lag_window") else paste0(v, "_lag", n)
    out <- dplyr::group_by(out, .data[[id_col]])
    if (mode == "index") {
      out <- dplyr::mutate(out, !!lag_nm := dplyr::lag(.data[[v]], n = n, default = NA))
    } else if (mode == "gap_aware") {
      out <- dplyr::mutate(out,
        !!lag_nm := ild_lag_gap_aware(.data[[v]], .data[[seq_col]], .data[[dt_col]], n, max_gap)
      )
    } else {
      out <- dplyr::mutate(out,
        !!lag_nm := ild_lag_time_window(.data[[v]], .data[[time_num_col]], window, resolution)
      )
    }
    out <- dplyr::ungroup(out)
  }
  restore_ild_attrs(x, out)
}

#' For one person's vector: lag by n rows, NA if any dt in the window > max_gap
#' @noRd
ild_lag_gap_aware <- function(val, seq, dt, n, max_gap) {
  len <- length(val)
  out <- rep(NA, len)
  for (i in (n + 1):len) {
    # intervals between row i-n and i: dt[i-n+1], ..., dt[i]
    start <- i - n
    dts <- dt[(start + 1):i]
    if (all(!is.na(dts)) && all(dts <= max_gap)) {
      out[i] <- val[start]
    }
  }
  out
}

#' For one person: time-window lag. For each row, observations in (time - window, time]; apply resolution.
#' @noRd
ild_lag_time_window <- function(val, time_num, window, resolution) {
  len <- length(val)
  out <- rep(NA_real_, len)
  for (i in seq_len(len)) {
    t_i <- time_num[i]
    if (is.na(t_i)) next
    lo <- t_i - window
    in_window <- which(time_num > lo & time_num < t_i)
    if (length(in_window) == 0) next
    if (resolution == "closest_prior" || resolution == "last_in_window") {
      idx <- in_window[which.max(time_num[in_window])]
      out[i] <- val[idx]
    } else {
      out[i] <- mean(val[in_window], na.rm = TRUE)
    }
  }
  out
}

#' Convert window to numeric (seconds for POSIXct). Accepts lubridate Period/Duration.
#' @param window Numeric, or Period, or Duration.
#' @param x ILD object (to infer time type if needed); currently only used if window is not numeric.
#' @noRd
ild_window_to_num <- function(window, x) {
  if (is.numeric(window)) return(as.numeric(window)[1L])
  if (inherits(window, "Period") || inherits(window, "Duration")) {
    return(as.numeric(lubridate::as.duration(window)))
  }
  as.numeric(window)[1]
}
