# Internal column names (reserved; never required from user)
.ILD_COLS <- c(
  ".ild_id",
  ".ild_time",
  ".ild_time_num",
  ".ild_seq",
  ".ild_dt",
  ".ild_gap"
)

# Attribute names set by ild_prepare(), read via ild_meta()
.ILD_ATTRS <- c(
  "ild_id",
  "ild_time",
  "ild_gap_threshold",
  "ild_n_units",
  "ild_n_obs",
  "ild_spacing"
)

#' Check if an object is a valid ILD tibble
#'
#' Returns TRUE if the object has all required `.ild_*` columns and
#' `ild_*` metadata attributes (as set by [ild_prepare()]).
#'
#' @param x Any object.
#' @return Logical.
#' @export
is_ild <- function(x) {
  if (!inherits(x, "data.frame")) return(FALSE)
  nms <- names(x)
  has_cols <- all(.ILD_COLS %in% nms)
  if (!has_cols) return(FALSE)
  attrs <- .ILD_ATTRS
  for (a in attrs) {
    if (is.null(attr(x, a, exact = TRUE))) return(FALSE)
  }
  TRUE
}

#' Validate an ILD object and error if invalid
#'
#' Checks presence and types of `.ild_*` columns and `ild_*` attributes.
#' Errors with a clear message if anything is missing or invalid.
#'
#' @param x Object to validate (expected to be an ILD tibble).
#' @return Invisibly returns `x` if valid.
#' @export
validate_ild <- function(x) {
  if (!inherits(x, "data.frame")) {
    stop("ILD object must be a data frame or tibble.", call. = FALSE)
  }
  nms <- names(x)
  missing_cols <- setdiff(.ILD_COLS, nms)
  if (length(missing_cols) > 0) {
    stop(
      "ILD object is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". Run ild_prepare() first.",
      call. = FALSE
    )
  }
  for (a in .ILD_ATTRS) {
    if (is.null(attr(x, a, exact = TRUE))) {
      stop("ILD object is missing attribute: ", a, ". Run ild_prepare() first.", call. = FALSE)
    }
  }
  # Basic type checks
  if (!is.numeric(x[[".ild_time_num"]])) {
    stop(".ild_time_num must be numeric.", call. = FALSE)
  }
  seq_vals <- x[[".ild_seq"]]
  if (!is.numeric(seq_vals) || any(is.na(seq_vals)) || any(seq_vals != as.integer(seq_vals))) {
    stop(".ild_seq must be integer-valued.", call. = FALSE)
  }
  if (!is.logical(x[[".ild_gap"]]) && !all(is.na(x[[".ild_gap"]]))) {
    stop(".ild_gap must be logical (or NA for first row per person).", call. = FALSE)
  }
  invisible(x)
}

#' Get ILD metadata attributes
#'
#' Returns the metadata attributes set by [ild_prepare()]: user-facing
#' id/time column names, gap threshold, n_units, n_obs, and spacing
#' (descriptive stats only).
#'
#' @param x An ILD object (see [is_ild()]).
#' @return A named list of metadata (ild_id, ild_time, ild_gap_threshold,
#'   ild_n_units, ild_n_obs, ild_spacing). \code{ild_spacing} includes overall
#'   stats and may contain \code{by_id}, a tibble of per-person spacing stats.
#' @export
ild_meta <- function(x) {
  validate_ild(x)
  out <- list(
    ild_id            = attr(x, "ild_id", exact = TRUE),
    ild_time          = attr(x, "ild_time", exact = TRUE),
    ild_gap_threshold = attr(x, "ild_gap_threshold", exact = TRUE),
    ild_n_units       = attr(x, "ild_n_units", exact = TRUE),
    ild_n_obs         = attr(x, "ild_n_obs", exact = TRUE),
    ild_spacing       = attr(x, "ild_spacing", exact = TRUE)
  )
  out
}

#' Internal constructor for ILD tibbles
#'
#' Builds the tibble with all `.ild_*` columns and sets all `ild_*`
#' attributes. Only used by ild_prepare(); not exported.
#'
#' @param data A data frame or tibble (already sorted, with user id/time and
#'   value columns plus the computed .ild_* columns).
#' @param ild_id Character; user-facing id column name.
#' @param ild_time Character; user-facing time column name.
#' @param gap_threshold Numeric; threshold used for .ild_gap.
#' @param spacing List; descriptive spacing stats (median, IQR, etc.).
#' @return A tibble with ILD attributes (validated).
#' @noRd
new_ild_df <- function(data, ild_id, ild_time, gap_threshold, spacing) {
  stopifnot(
    is.character(ild_id), length(ild_id) == 1,
    is.character(ild_time), length(ild_time) == 1,
    is.numeric(gap_threshold), length(gap_threshold) == 1,
    is.list(spacing)
  )
  n_units <- length(unique(data[[".ild_id"]]))
  n_obs   <- nrow(data)
  x <- tibble::as_tibble(data)
  attr(x, "ild_id")            <- ild_id
  attr(x, "ild_time")          <- ild_time
  attr(x, "ild_gap_threshold") <- gap_threshold
  attr(x, "ild_n_units")       <- n_units
  attr(x, "ild_n_obs")         <- n_obs
  attr(x, "ild_spacing")       <- spacing
  class(x) <- c("ild_tbl", class(x))
  x
}

#' Coerce to ILD object
#'
#' If the object already has the required `.ild_*` columns and
#' attributes, validates and returns it (with ild_tbl class if missing).
#' Otherwise errors.
#'
#' @param x A data frame or tibble that may already be ILD-shaped.
#' @return An ILD tibble (invisibly validated).
#' @export
as_ild <- function(x) {
  validate_ild(x)
  if (!inherits(x, "ild_tbl")) {
    class(x) <- c("ild_tbl", class(x))
  }
  x
}

#' Copy ILD attributes and class from one object to another (internal)
#' @noRd
restore_ild_attrs <- function(from, to) {
  for (a in .ILD_ATTRS) attr(to, a) <- attr(from, a, exact = TRUE)
  class(to) <- class(from)
  to
}

# Satisfy R CMD check (tidy eval and rlang)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data", ".ild_time_num", ":="))
}
