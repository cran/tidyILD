#' Within-person and between-person decomposition (centering)
#'
#' For each selected variable, computes the person mean (between-person
#' component) and the within-person deviation (variable minus person mean).
#' Use `*_wp` at level-1 and `*_bp` at level-2 or in cross-level interactions
#' to avoid ecological fallacy and conflation bias.
#'
#' @param x An ILD object (see [is_ild()]).
#' @param ... Variables to center (tidy-select). Unquoted names or a single
#'   character vector of column names.
#' @param type Character. `"person_mean"` (default) for person-mean centering
#'   (x_bp, x_wp); `"grand_mean"` for grand-mean centering (x_gm, x_wp_gm).
#' @return The same ILD tibble with additional columns: for each variable
#'   `v`, `v_bp` (person mean), `v_wp` (v - v_bp). If `type = "grand_mean"`,
#'   also `v_gm` and optionally `v_wp_gm`. ILD attributes are preserved.
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom rlang .data
#' @export
ild_center <- function(x, ..., type = c("person_mean", "grand_mean")) {
  validate_ild(x)
  type <- match.arg(type)
  vars <- names(dplyr::select(x, ...))
  if (length(vars) == 0) stop("No variables selected for centering.", call. = FALSE)
  id_col <- ".ild_id"
  out <- x
  for (v in vars) {
    if (!v %in% names(out)) stop("Variable '", v, "' not found in data.", call. = FALSE)
    out <- dplyr::group_by(out, .data[[id_col]])
    out <- dplyr::mutate(out,
      !!paste0(v, "_bp") := mean(.data[[v]], na.rm = TRUE),
      !!paste0(v, "_wp") := .data[[v]] - mean(.data[[v]], na.rm = TRUE)
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
