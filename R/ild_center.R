#' Within-person and between-person decomposition (centering)
#'
#' For each selected variable, computes the person mean (between-person
#' component) and the within-person deviation (variable minus person mean).
#' Use `*_wp` at level-1 and `*_bp` at level-2 or in cross-level interactions
#' to avoid ecological fallacy and conflation bias. Selected variables must be numeric.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param ... Variables to center (tidy-select). Unquoted names or a single
#'   character vector of column names. Must be numeric.
#' @param type Character. `"person_mean"` (default) for person-mean centering
#'   (x_bp, x_wp); `"grand_mean"` for grand-mean centering (x_gm, x_wp_gm).
#' @param naming Character. `"suffix"` (default): new columns \code{var_bp}, \code{var_wp};
#'   \code{"prefix"}: new columns \code{BP_var}, \code{WP_var}.
#' @return The same ILD tibble with additional columns. ILD attributes are preserved.
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom rlang .data
#' @export
ild_center <- function(x, ..., type = c("person_mean", "grand_mean"),
                      naming = c("suffix", "prefix")) {
  validate_ild(x)
  type <- match.arg(type)
  naming <- match.arg(naming)
  vars <- names(dplyr::select(x, ...))
  if (length(vars) == 0) stop("No variables selected for centering.", call. = FALSE)
  id_col <- ".ild_id"
  for (v in vars) {
    if (!v %in% names(x)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    if (!is.numeric(x[[v]])) {
      stop("Variable '", v, "' is not numeric. Centering requires numeric variables.", call. = FALSE)
    }
  }
  out <- x
  for (v in vars) {
    out <- dplyr::group_by(out, .data[[id_col]])
    if (naming == "suffix") {
      bp_nm <- paste0(v, "_bp")
      wp_nm <- paste0(v, "_wp")
    } else {
      bp_nm <- paste0("BP_", v)
      wp_nm <- paste0("WP_", v)
    }
    out <- dplyr::mutate(out,
      !!bp_nm := mean(.data[[v]], na.rm = TRUE),
      !!wp_nm := .data[[v]] - mean(.data[[v]], na.rm = TRUE)
    )
    out <- dplyr::ungroup(out)
    if (type == "grand_mean") {
      gm <- mean(out[[v]], na.rm = TRUE)
      out[[paste0(v, "_gm")]] <- gm
      out[[paste0(v, "_wp_gm")]] <- out[[v]] - gm
    }
  }
  restore_ild_attrs(x, out)
}
