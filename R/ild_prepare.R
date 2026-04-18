#' Prepare a data frame as an ILD (intensive longitudinal data) object
#'
#' Validates and encodes longitudinal structure: parses time, sorts by id and
#' time, handles duplicate timestamps, and adds internal columns (`.ild_*`)
#' and metadata. All downstream functions assume the result of `ild_prepare()`.
#'
#' @param data A data frame or tibble with at least an id and a time column.
#'   If \code{id} and \code{time} are both omitted, \code{data} must be a
#'   \code{tbl_ts} from \pkg{tsibble} (requires a single key column and an index);
#'   key and index names are inferred (see [ild_as_tsibble()] for the reverse).
#'
#'   \strong{Tsibble interoperability:} Accepting a \code{tbl_ts} is an input
#'   convenience; the result is always a plain ILD tibble (the \code{tbl_ts} class
#'   is dropped). Metadata from the source tsibble (key, index, interval summary,
#'   regularity) is stored in \code{attr(x, "tidyILD")$tsibble} when available;
#'   use [ild_tsibble_meta()] to read it. Use [ild_as_tsibble()] for a
#'   best-effort round-trip. \strong{Conceptual choice (keys):} ILD uses one
#'   subject identifier per row; \code{tbl_ts} must have exactly one key
#'   column. Compound keys are not supported—combine levels into a single id
#'   column before calling \code{ild_prepare()}.
#' @param id Character. Name of the subject/unit identifier column. Omit both
#'   \code{id} and \code{time} when \code{data} is a tsibble (see above).
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
#' @seealso \href{../doc/tsibble-interoperability.html}{\code{vignette("tsibble-interoperability", package = "tidyILD")}}
#'   (tsibble ingestion, provenance, \code{ild_as_tsibble()}).
#' @importFrom dplyr arrange group_by mutate n row_number summarise ungroup
#' @importFrom tibble as_tibble
#' @export
ild_prepare <- function(data,
                        id = NULL,
                        time = NULL,
                        gap_threshold = Inf,
                        duplicate_handling = c("first", "last", "error", "collapse"),
                        collapse_fn = NULL) {
  duplicate_handling <- match.arg(duplicate_handling)
  if (duplicate_handling == "collapse" && (is.null(collapse_fn) || !is.list(collapse_fn))) {
    stop("When duplicate_handling = 'collapse', provide a named list of functions in collapse_fn (e.g. list(x = mean)).", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("'data' must be a data frame or tibble.", call. = FALSE)
  ts_meta <- NULL
  if (xor(is.null(id), is.null(time))) {
    stop("Provide both id and time, or omit both and pass a tsibble (tbl_ts) with one key and an index.", call. = FALSE)
  }
  if (is.null(id) && is.null(time)) {
    if (!requireNamespace("tsibble", quietly = TRUE)) {
      stop("Package 'tsibble' is required when id and time are omitted. Install it or supply id and time explicitly.", call. = FALSE)
    }
    if (!inherits(data, "tbl_ts")) {
      stop("Provide both id and time, or omit both and pass a tsibble (tbl_ts) with one key and an index.", call. = FALSE)
    }
    kv <- tsibble::key_vars(data)
    iv <- tsibble::index_var(data)
    if (length(kv) != 1L) {
      stop("tsibble must have exactly one key column for ild_prepare (subject id); found ", length(kv), ".", call. = FALSE)
    }
    id <- kv[1L]
    time <- iv
    ts_meta <- ild_tsibble_capture_meta(data)
    data <- tibble::as_tibble(data)
  }
  id <- as.character(id)[1]
  time <- as.character(time)[1]
  if (!id %in% names(data)) stop("Column '", id, "' not found in data.", call. = FALSE)
  if (!time %in% names(data)) stop("Column '", time, "' not found in data.", call. = FALSE)

  if (is.null(ts_meta) && requireNamespace("tsibble", quietly = TRUE) && inherits(data, "tbl_ts")) {
    ts_meta <- ild_tsibble_capture_meta(data)
  }

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
  if (!is.null(ts_meta)) {
    spacing <- ild_spacing_tsibble_bridge(spacing, ts_meta)
  }

  out <- new_ild_df(
    data = out,
    ild_id = id,
    ild_time = time,
    gap_threshold = gap_threshold,
    spacing = spacing,
    tsibble_meta = ts_meta
  )
  out <- ild_add_step(out, "ild_prepare",
    list(id = id, time = time, gap_threshold = gap_threshold, duplicate_handling = duplicate_handling),
    list(
      n_id = attr(out, "ild_n_units", exact = TRUE),
      n_obs = attr(out, "ild_n_obs", exact = TRUE),
      spacing_class = ild_spacing_class(out),
      source_was_tsibble = !is.null(ts_meta),
      tsibble_interval_declared = if (!is.null(ts_meta)) ts_meta$interval_format else NULL
    )
  )
  out
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
