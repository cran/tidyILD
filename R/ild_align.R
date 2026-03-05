#' Align a secondary stream to primary ILD within a time window
#'
#' For each row in the primary ILD, finds observations in the secondary
#' data set (same id, time within \code{window} before the primary time)
#' and attaches an aggregated value (e.g. mean, median, or closest).
#' Use when combining self-report with wearables or other streams that
#' have different timestamps.
#'
#' @param primary An ILD object (see [is_ild()]); the stream to keep as rows.
#' @param secondary A data frame with id and time columns and the value variable(s) to align.
#' @param value_var Character. Name of the column in \code{secondary} to align (e.g. \code{"heart_rate"}).
#' @param window Numeric or lubridate duration. Time window (same units as \code{.ild_time_num},
#'   e.g. seconds for POSIXct). Only secondary observations with time in
#'   \code{(primary_time - window, primary_time]} are used.
#' @param time_secondary Character. Name of the time column in \code{secondary} (default \code{"time"}).
#' @param fun Character. Aggregation for values in window: \code{"mean"}, \code{"median"}, or \code{"closest"} (most recent in window).
#' @return The primary data with a new column \code{<value_var>_aligned} (numeric; NA where no secondary obs in window).
#' @export
#' @examples
#' prim <- ild_prepare(
#'   data.frame(
#'     id = rep(1:2, each = 3),
#'     time = as.POSIXct(rep(c(0, 3600, 7200), 2), origin = "1970-01-01"),
#'     y = rnorm(6)
#'   ),
#'   id = "id", time = "time"
#' )
#' sec <- data.frame(
#'   id = rep(1:2, each = 4),
#'   time = as.POSIXct(rep(c(0, 1800, 3600, 5400), 2), origin = "1970-01-01"),
#'   heart_rate = 60 + rnorm(8, 0, 5)
#' )
#' ild_align(prim, sec, "heart_rate", window = 3600, fun = "mean")
ild_align <- function(primary, secondary, value_var, window,
                     time_secondary = "time", fun = c("mean", "median", "closest")) {
  validate_ild(primary)
  fun <- match.arg(fun)
  meta <- ild_meta(primary)
  id_name <- meta$ild_id
  if (!id_name %in% names(secondary)) stop("secondary must have id column '", id_name, "'.", call. = FALSE)
  if (!time_secondary %in% names(secondary)) stop("secondary must have time column '", time_secondary, "'.", call. = FALSE)
  if (!value_var %in% names(secondary)) stop("secondary must have value column '", value_var, "'.", call. = FALSE)
  window_num <- if (is.numeric(window)) as.numeric(window)[1] else {
    if (inherits(window, "Period") || inherits(window, "Duration")) as.numeric(lubridate::as.duration(window)) else as.numeric(window)[1]
  }
  t_sec_num <- ild_time_to_num(secondary[[time_secondary]])
  primary_id <- primary[[".ild_id"]]
  primary_t <- primary[[".ild_time_num"]]
  n <- nrow(primary)
  aligned <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    pid <- primary_id[i]
    pt <- primary_t[i]
    if (is.na(pt)) next
    lo <- pt - window_num
    idx <- which(secondary[[id_name]] == pid & t_sec_num > lo & t_sec_num <= pt)
    if (length(idx) == 0) next
    if (fun == "closest") {
      imax <- idx[which.max(t_sec_num[idx])]
      aligned[i] <- secondary[[value_var]][imax]
    } else {
      vals <- secondary[[value_var]][idx]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) next
      if (fun == "mean") aligned[i] <- mean(vals) else aligned[i] <- stats::median(vals)
    }
  }
  out <- tibble::as_tibble(primary)
  out[[paste0(value_var, "_aligned")]] <- aligned
  restore_ild_attrs(primary, out)
}
