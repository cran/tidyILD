# Internal helpers shared by ild_ipw_weights, ild_iptw_weights, ild_ipcw_weights, ild_joint_msm_weights

#' Trim numeric weights at quantiles
#' @param w Numeric vector of weights.
#' @param trim Numeric of length 2, quantiles (e.g. \code{c(0.01, 0.99)}).
#' @return \code{w} with values clipped to quantile bounds.
#' @noRd
.ild_ipw_trim <- function(w, trim = c(0.01, 0.99)) {
  if (length(trim) < 2L || !is.numeric(trim)) return(w)
  qq <- stats::quantile(w, probs = trim, na.rm = TRUE)
  pmin(pmax(w, qq[1L]), qq[2L])
}

#' Restore ILD column attributes after adding a column (in-place style)
#' @param x ILD object.
#' @param attrs Result of \code{attributes(x)} before modification.
#' @return \code{x} with attributes restored.
#' @noRd
.ild_ipw_restore_attrs <- function(x, attrs) {
  for (a in c("ild_id", "ild_time", "ild_gap_threshold", "ild_n_units", "ild_n_obs", "ild_spacing", "class", "tidyILD")) {
    if (!is.null(attrs[[a]])) attr(x, a) <- attrs[[a]]
  }
  if (!is.null(attrs$names)) attr(x, "names") <- names(x)
  x
}
