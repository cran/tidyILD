# tsibble interoperability: capture provenance on input, optional round-trip via ild_as_tsibble()

#' Capture tsibble metadata before coercion to plain tibble (internal)
#' @keywords internal
#' @noRd
ild_tsibble_capture_meta <- function(data) {
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    return(NULL)
  }
  if (!inherits(data, "tbl_ts")) {
    return(NULL)
  }
  kv <- tsibble::key_vars(data)
  iv <- tsibble::index_var(data)
  intv <- tryCatch(tsibble::interval(data), error = function(e) NULL)
  int_txt <- tryCatch({
    if (!is.null(intv)) format(intv) else NA_character_
  }, error = function(e) NA_character_)
  reg <- tryCatch(tsibble::is_regular(data), error = function(e) NA)
  idx_vec <- tryCatch(tsibble::index(data), error = function(e) NULL)
  idx_class <- if (!is.null(idx_vec)) class(idx_vec)[[1L]] else NA_character_
  list(
    source_class = "tbl_ts",
    key_vars = kv,
    index_var = iv,
    interval_format = int_txt,
    is_regular = reg,
    tsibble_version = tryCatch(
      as.character(utils::packageVersion("tsibble")),
      error = function(e) NA_character_
    ),
    index_type = idx_class
  )
}

#' Merge tsibble declaration with empirical ILD spacing (internal)
#' @keywords internal
#' @noRd
ild_spacing_tsibble_bridge <- function(spacing, ts_meta) {
  if (is.null(ts_meta)) {
    return(spacing)
  }
  spacing$tsibble <- list(
    declared_interval = ts_meta$interval_format,
    key_vars = ts_meta$key_vars,
    index_var = ts_meta$index_var,
    tsibble_was_regular = ts_meta$is_regular,
    median_dt_sec_empirical = spacing$median_dt,
    iqr_dt_sec_empirical = spacing$iqr_dt,
    note = paste0(
      "Declared tsibble interval/regularity reflects the input tbl_ts before ild_prepare(). ",
      "Empirical median_dt / iqr_dt are from .ild_dt after sorting and duplicate handling; ",
      "they may differ if rows were dropped or timestamps changed."
    )
  )
  spacing
}

#' Tsibble provenance from an ILD object
#'
#' If [ild_prepare()] was called on a \code{tbl_ts}, metadata from the source
#' tsibble (key, index, interval summary, regularity) is stored in
#' \code{attr(x, "tidyILD")$tsibble}. Returns \code{NULL} if the object was
#' not prepared from a tsibble.
#'
#' @param x An object that passes [validate_ild()].
#' @return A named list or \code{NULL}.
#' @seealso [ild_prepare()], [ild_as_tsibble()],
#'   \href{../doc/tsibble-interoperability.html}{\code{vignette("tsibble-interoperability", package = "tidyILD")}}
#' @export
ild_tsibble_meta <- function(x) {
  validate_ild(x)
  b <- attr(x, "tidyILD", exact = TRUE)
  if (is.null(b) || is.null(b$tsibble)) {
    return(NULL)
  }
  b$tsibble
}

#' Convert an ILD object to a tsibble
#'
#' Wraps [tsibble::as_tsibble()] using the subject and time column names from
#' [ild_meta()] as key and index. When [ild_prepare()] recorded tsibble
#' provenance (\code{ild_tsibble_meta()} is non-\code{NULL}), the \code{regular}
#' argument is set from the stored \code{is_regular} flag so the reconstructed
#' \code{interval} often matches the original for unchanged data. Otherwise
#' defaults to \code{regular = TRUE}.
#'
#' @param x An object that passes [validate_ild()].
#' @param ... Optional arguments passed to [tsibble::as_tsibble()] (e.g.
#'   override \code{regular}).
#' @return A \code{tbl_ts} object (see \pkg{tsibble}).
#' @examples
#' \donttest{
#' d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' if (requireNamespace("tsibble", quietly = TRUE)) {
#'   ts <- ild_as_tsibble(x)
#'   class(ts)
#' }
#' }
#' @seealso \href{../doc/tsibble-interoperability.html}{\code{vignette("tsibble-interoperability", package = "tidyILD")}}
#' @importFrom rlang inject sym
#' @export
ild_as_tsibble <- function(x, ...) {
  validate_ild(x)
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    stop("Package 'tsibble' is required for ild_as_tsibble(). Install it with install.packages(\"tsibble\").", call. = FALSE)
  }
  m <- ild_meta(x)
  id_col <- m$ild_id
  time_col <- m$ild_time
  dots <- list(...)
  regular_arg <- dots$regular
  dots$regular <- NULL
  tsm <- ild_tsibble_meta(x)
  if (!is.null(regular_arg)) {
    regular_use <- regular_arg
  } else if (!is.null(tsm) && is.logical(tsm$is_regular) && length(tsm$is_regular) == 1L && !is.na(tsm$is_regular)) {
    regular_use <- tsm$is_regular
  } else {
    regular_use <- TRUE
  }
  out <- rlang::inject(tsibble::as_tsibble(
    tibble::as_tibble(x),
    key = !!rlang::sym(id_col),
    index = !!rlang::sym(time_col),
    regular = regular_use,
    !!!dots
  ))
  if (!is.null(tsm) && isTRUE(tsm$is_regular) && !isTRUE(tsibble::is_regular(out))) {
    warning(
      "ild_as_tsibble(): reconstructed tsibble is not regular; empirical spacing may differ from the original tbl_ts (sorting, duplicates, or row drops in ild_prepare).",
      call. = FALSE
    )
  }
  out
}
