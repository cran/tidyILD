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

# Optional primary metadata bundle; when present, ild_meta() can read from it
.TIDYILD_ATTR <- "tidyILD"

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

#' Upgrade legacy ILD object to tidyILD schema (internal)
#'
#' If \code{attr(x, "tidyILD")} is missing but \code{ild_*} attributes exist,
#' builds the tidyILD bundle and sets it; adds \code{tidyild_df} to class.
#' Emits a one-time warning. Never modifies user columns.
#' @param x An object that may be a legacy ILD tibble.
#' @return \code{x} (possibly with \code{attr(x, "tidyILD")} and class updated).
#' @noRd
ild_normalize_internal <- function(x) {
  if (!inherits(x, "data.frame")) return(x)
  if (!is.null(attr(x, .TIDYILD_ATTR, exact = TRUE))) return(x)
  ild_id <- attr(x, "ild_id", exact = TRUE)
  ild_time <- attr(x, "ild_time", exact = TRUE)
  ild_gap_threshold <- attr(x, "ild_gap_threshold", exact = TRUE)
  ild_spacing <- attr(x, "ild_spacing", exact = TRUE)
  if (is.null(ild_id) || is.null(ild_time) || is.null(ild_gap_threshold)) return(x)
  pv <- tryCatch(as.character(utils::packageVersion("tidyILD")), error = function(e) "0.0.0")
  attr(x, .TIDYILD_ATTR) <- list(
    id_col = ild_id,
    time_col = ild_time,
    tz = "UTC",
    gap_threshold = ild_gap_threshold,
    created = NA,
    spacing = ild_spacing,
    provenance = list(version = pv, schema_version = "1", object_type = "ild_data", steps = list())
  )
  if (!inherits(x, "tidyild_df")) {
    class(x) <- c("tidyild_df", "ild_tbl", setdiff(class(x), c("tidyild_df", "ild_tbl")))
  }
  if (is.null(getOption("tidyILD.schema_upgraded", NULL))) {
    warning(
      "ILD internal schema upgraded: attr(x, \"tidyILD\") and class \"tidyild_df\" added. ",
      "This warning is shown once per session.",
      call. = FALSE
    )
    options(tidyILD.schema_upgraded = TRUE)
  }
  x
}

#' Validate an ILD object and error if invalid
#'
#' Checks presence and types of `.ild_*` columns and `ild_*` attributes.
#' Errors with a clear message if anything is missing or invalid.
#' Calls \code{ild_normalize_internal()} so legacy objects get \code{attr(x, "tidyILD")} and class \code{tidyild_df}.
#'
#' @param x Object to validate (expected to be an ILD tibble).
#' @return Invisibly returns \code{x} if valid.
#' @export
validate_ild <- function(x) {
  ild_normalize_internal(x)
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
#' attributes plus \code{attr(x, "tidyILD")}. Only used by ild_prepare(); not exported.
#'
#' @param data A data frame or tibble (already sorted, with user id/time and
#'   value columns plus the computed .ild_* columns).
#' @param ild_id Character; user-facing id column name.
#' @param ild_time Character; user-facing time column name.
#' @param gap_threshold Numeric; threshold used for .ild_gap.
#' @param spacing List; descriptive spacing stats (median, IQR, etc.).
#' @param tz Character; time zone for the bundle (default \code{"UTC"}).
#' @param tsibble_meta Optional list from \code{ild_tsibble_capture_meta()} when
#'   \code{data} was a \code{tbl_ts}; stored under \code{attr(x, "tidyILD")$tsibble}.
#' @return A tibble with class \code{tidyild_df}, \code{ild_tbl}, and ILD attributes.
#' @noRd
new_ild_df <- function(data, ild_id, ild_time, gap_threshold, spacing, tz = "UTC", tsibble_meta = NULL) {
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
  pv <- tryCatch(as.character(utils::packageVersion("tidyILD")), error = function(e) "0.0.0")
  tidy_bundle <- list(
    id_col = ild_id,
    time_col = ild_time,
    tz = tz,
    gap_threshold = gap_threshold,
    created = Sys.time(),
    spacing = spacing,
    provenance = list(version = pv, schema_version = "1", object_type = "ild_data", steps = list())
  )
  if (!is.null(tsibble_meta)) {
    tidy_bundle$tsibble <- tsibble_meta
  }
  attr(x, .TIDYILD_ATTR) <- tidy_bundle
  class(x) <- c("tidyild_df", "ild_tbl", class(x))
  x
}

#' Coerce to ILD object
#'
#' If the object already has the required `.ild_*` columns and
#' attributes, validates and returns it (with \code{tidyild_df} and \code{ild_tbl} class if missing).
#' Otherwise errors.
#'
#' @param x A data frame or tibble that may already be ILD-shaped.
#' @return An ILD tibble with class \code{tidyild_df} and \code{ild_tbl}.
#' @export
as_ild <- function(x) {
  validate_ild(x)
  if (!inherits(x, "tidyild_df")) {
    class(x) <- c("tidyild_df", "ild_tbl", setdiff(class(x), c("tidyild_df", "ild_tbl")))
  }
  if (!inherits(x, "ild_tbl")) {
    class(x) <- c("ild_tbl", class(x))
  }
  x
}

#' Copy ILD attributes and class from one object to another (internal)
#' @noRd
restore_ild_attrs <- function(from, to) {
  for (a in .ILD_ATTRS) attr(to, a) <- attr(from, a, exact = TRUE)
  tidyild <- attr(from, .TIDYILD_ATTR, exact = TRUE)
  if (!is.null(tidyild)) attr(to, .TIDYILD_ATTR) <- tidyild
  class(to) <- class(from)
  to
}

# Satisfy R CMD check (tidy eval and rlang)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data", ".ild_time_num", ":=", "y"))
}
