#' Prepare a data frame as an ILD (intensive longitudinal data) object
#'
#' Validates and encodes longitudinal structure: parses time, sorts by id and
#' time, handles duplicate timestamps, and adds internal columns (`.ild_*`)
#' and metadata. All downstream functions assume the result of `ild_prepare()`.
#'
#' @param data A data frame or tibble with at least an id and a time column.
#' @param id Character. Name of the subject/unit identifier column.
#' @param time Character. Name of the time column (Date, POSIXct, or numeric).
#' @param gap_threshold Numeric. Time distance above which an interval is
#'   flagged as a gap (`.ild_gap` TRUE). Same units as the numeric time
#'   (e.g. seconds if time is POSIXct). Use `Inf` to disable gap flagging.
#' @param duplicate_handling Character. How to handle duplicate timestamps
#'   within the same id: `"first"` (keep first), `"last"` (keep last),
#'   `"error"` (stop with an error), `"collapse"` (aggregate with \code{collapse_fn}).
#' @param collapse_fn Named list of functions, one per variable to collapse.
#'   Used only when \code{duplicate_handling = "collapse"}. E.g. \code{list(x = mean, y = function(z) z[1])}.
#'   Variables not in \code{collapse_fn} keep their first value within the duplicate group.
#' @return An ILD tibble with `.ild_*` columns and metadata attributes.
#'   Spacing metadata (see [ild_meta()]) includes overall stats and a
#'   \code{by_id} tibble of per-person spacing stats (median_dt, iqr_dt,
#'   n_intervals, pct_gap). Use [ild_summary()] to inspect and check gap
#'   flags before modeling.
#' @importFrom dplyr arrange group_by mutate n row_number summarise ungroup
#' @importFrom tibble as_tibble
#' @export
ild_prepare <- function(data,
                        id,
                        time,
                        gap_threshold = Inf,
                        duplicate_handling = c("first", "last", "error", "collapse"),
                        collapse_fn = NULL) {
  duplicate_handling <- match.arg(duplicate_handling)
  if (duplicate_handling == "collapse" && (is.null(collapse_fn) || !is.list(collapse_fn))) {
    stop("When duplicate_handling = 'collapse', provide a named list of functions in collapse_fn (e.g. list(x = mean)).", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("'data' must be a data frame or tibble.", call. = FALSE)
  id <- as.character(id)[1]
  time <- as.character(time)[1]
  if (!id %in% names(data)) stop("Column '", id, "' not found in data.", call. = FALSE)
  if (!time %in% names(data)) stop("Column '", time, "' not found in data.", call. = FALSE)

  # Avoid masking user columns: work on a copy and add .ild_* columns
  out <- tibble::as_tibble(data)
  out[[".ild_id"]]   <- out[[id]]
  out[[".ild_time"]] <- out[[time]]
  out[[".ild_time_num"]] <- ild_time_to_num(out[[time]])

  # Sort by id then time
  out <- dplyr::arrange(out, .data[[".ild_id"]], .data[[".ild_time_num"]])

  # Duplicate handling: within each id, keep first/last per timestamp, error, or collapse
  out <- ild_handle_duplicates(out, duplicate_handling, collapse_fn)

  # Within-person sequence and intervals
  out <- dplyr::group_by(out, .data[[".ild_id"]])
  out <- dplyr::mutate(out,
    .ild_seq = seq_len(dplyr::n()),
    .ild_dt = .ild_time_num - dplyr::lag(.ild_time_num, default = NA_real_),
    .ild_gap = dplyr::if_else(
      is.na(.data[[".ild_dt"]]),
      NA,
      .data[[".ild_dt"]] > gap_threshold
    )
  )
  out <- dplyr::ungroup(out)

  # Descriptive spacing stats (no hard classifier)
  dt_vals <- out[[".ild_dt"]]
  spacing <- ild_spacing_stats(dt_vals, gap_threshold = if (is.finite(gap_threshold)) gap_threshold else NULL)
  spacing$by_id <- ild_spacing_by_id(out, ".ild_id", ".ild_dt",
    gap_threshold = if (is.finite(gap_threshold)) gap_threshold else NULL)

  new_ild_df(
    data = out,
    ild_id = id,
    ild_time = time,
    gap_threshold = gap_threshold,
    spacing = spacing
  )
}

#' Within each .ild_id, resolve duplicates by keeping first/last row per .ild_time_num, error, or collapse
#' @noRd
ild_handle_duplicates <- function(data, handling, collapse_fn = NULL) {
  id_col <- ".ild_id"
  time_col <- ".ild_time_num"
  grp <- dplyr::group_by(data, .data[[id_col]], .data[[time_col]])
  ns <- dplyr::summarise(grp, .n = dplyr::n(), .groups = "drop")
  if (handling == "error" && any(ns$.n > 1)) {
    stop("Duplicate timestamps within the same id found. Set duplicate_handling to 'first', 'last', or 'collapse'.", call. = FALSE)
  }
  if (handling == "error") return(dplyr::ungroup(dplyr::group_by(data, .data[[id_col]], .data[[time_col]])))
  if (handling == "collapse") {
    fn_list <- collapse_fn
    grp <- dplyr::group_by(data, .data[[id_col]], .data[[time_col]])
    out <- dplyr::summarise(grp, dplyr::across(
      dplyr::everything(),
      function(col) {
        nm <- dplyr::cur_column()
        if (nm %in% names(fn_list)) fn_list[[nm]](col) else col[1]
      }
    ), .groups = "drop")
    return(out)
  }
  grp <- dplyr::group_by(data, .data[[id_col]], .data[[time_col]])
  grp <- dplyr::mutate(grp,
    .ild_dup_rn = dplyr::row_number(),
    .ild_dup_n = dplyr::n()
  )
  grp <- dplyr::ungroup(grp)
  keep <- if (handling == "first") {
    grp[[".ild_dup_rn"]] == 1L
  } else {
    grp[[".ild_dup_rn"]] == grp[[".ild_dup_n"]]
  }
  out <- grp[keep, ]
  out[[".ild_dup_rn"]] <- NULL
  out[[".ild_dup_n"]] <- NULL
  out
}
