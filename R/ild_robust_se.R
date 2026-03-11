#' Cluster-robust variance-covariance matrix for ILD model fits
#'
#' Computes cluster-robust (sandwich) variance estimators with small-sample
#' corrections via the \pkg{clubSandwich} package. Use with [tidy_ild_model()]
#' via \code{se = "robust"} for fixed-effect inference. Requires
#' \code{attr(fit, "ild_data")}; refit with [ild_lme()] if missing.
#'
#' @param fit A fitted model from [ild_lme()] (lmerMod or lme).
#' @param type Character. Correction type: \code{"CR2"} (recommended), \code{"CR3"}, or \code{"CR0"}.
#' @param cluster Either \code{"id"} (default; cluster by the ILD id column from
#'   \code{ild_data}) or \code{"data"} to use a user-supplied vector via \code{cluster_vec}.
#' @param cluster_vec When \code{cluster = "data"}, a vector of cluster IDs aligned to
#'   the model rows (same length and order as \code{attr(fit, "ild_data")}).
#' @param ... Passed to \code{clubSandwich::vcovCR()}.
#' @return A list with \code{vcov} (matrix), \code{type}, \code{cluster_name},
#'   \code{engine} (\code{"lmer"} or \code{"lme"}), and optionally \code{message}
#'   if a fallback was used (e.g. lme not fully supported on this build).
#' @seealso [tidy_ild_model()] with \code{se = "robust"}, \code{clubSandwich::vcovCR}.
#' @examples
#' if (requireNamespace("clubSandwich", quietly = TRUE)) {
#'   set.seed(1)
#'   dat <- ild_simulate(n_id = 8, n_obs_per = 6, seed = 1)
#'   dat <- ild_prepare(dat, id = "id", time = "time")
#'   dat <- ild_center(dat, y)
#'   fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = dat, ar1 = FALSE, warn_no_ar1 = FALSE)
#'   rv <- ild_robust_se(fit, type = "CR2")
#'   rv$engine
#'   dim(rv$vcov)
#' }
#' @export
ild_robust_se <- function(fit, type = c("CR2", "CR3", "CR0"), cluster = c("id", "data"), cluster_vec = NULL, ...) {
  type <- match.arg(type)
  cluster <- match.arg(cluster)
  rlang::check_installed("clubSandwich", reason = "to use cluster-robust standard errors")
  data <- attr(fit, "ild_data", exact = TRUE)
  if (is.null(data)) {
    stop("Fit was not produced by tidyILD; refit using ild_lme() so the fit carries ild_data.",
         call. = FALSE)
  }
  validate_ild(data)
  engine <- if (inherits(fit, "lme")) "lme" else "lmer"
  cluster_vec <- if (cluster == "id") {
    meta <- ild_meta(data)
    id_name <- meta$ild_id
    if (!id_name %in% names(data)) stop("ild_data must contain id column '", id_name, "'.", call. = FALSE)
    data[[id_name]]
  } else {
    if (is.null(cluster_vec)) stop("When cluster = \"data\", supply cluster_vec.", call. = FALSE)
    if (length(cluster_vec) != nrow(data)) {
      stop("cluster_vec length (", length(cluster_vec), ") must match ild_data rows (", nrow(data), ").", call. = FALSE)
    }
    cluster_vec
  }
  cluster_name <- if (cluster == "id") ild_meta(data)$ild_id else "user"
  out <- tryCatch({
    vc <- clubSandwich::vcovCR(fit, cluster = cluster_vec, type = type, ...)
    list(vcov = vc, type = type, cluster_name = cluster_name, engine = engine)
  }, error = function(e) {
    list(
      vcov = stats::vcov(fit),
      type = type,
      cluster_name = cluster_name,
      engine = engine,
      message = paste0("clubSandwich::vcovCR failed (", conditionMessage(e), "); returning model vcov.")
    )
  })
  if (!is.null(out$message)) {
    warning(out$message, call. = FALSE)
  }
  out
}
