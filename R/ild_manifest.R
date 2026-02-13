#' Create a reproducibility manifest
#'
#' Captures timestamp, optional seed, optional scenario fingerprint,
#' session info, and optional git SHA for use when saving or serializing
#' results (e.g. after [ild_lme()] or [ild_diagnostics()]). The return
#' value is a serializable list suitable for [saveRDS()] or [ild_bundle()].
#'
#' @param seed Optional integer. Seed used for the run (e.g. from
#'   [ild_simulate()] or set before fitting). Not captured automatically;
#'   pass explicitly if you want it in the manifest.
#' @param scenario Optional. Named list or character string describing the
#'   run (e.g. formula, n_obs, n_id, ar1). Build from [ild_summary()] or
#'   a short list when calling after [ild_lme()] / [ild_diagnostics()].
#' @param include_session Logical. If `TRUE` (default), include
#'   [utils::sessionInfo()] in the manifest. Set to `FALSE` to reduce size.
#' @param include_git Logical. If `TRUE`, attempt to record the current
#'   git commit SHA from \code{git_path}. Default `FALSE`.
#' @param git_path Character. Path to the repository root (default
#'   \code{"."}). Used only when \code{include_git = TRUE}.
#' @return A list with elements \code{timestamp} (POSIXct), \code{seed}
#'   (integer or NULL), \code{scenario} (as provided or NULL),
#'   \code{session_info} (list from sessionInfo() or NULL),
#'   \code{git_sha} (length-1 character or NA). All elements are
#'   serializable.
#' @examples
#' m <- ild_manifest()
#' names(m)
#' m <- ild_manifest(seed = 42, scenario = list(n_obs = 100, formula = "y ~ x"))
#' m$seed
#' m$scenario
#' @export
ild_manifest <- function(seed = NULL,
                        scenario = NULL,
                        include_session = TRUE,
                        include_git = FALSE,
                        git_path = ".") {
  timestamp <- Sys.time()
  session_info <- if (include_session) utils::sessionInfo() else NULL
  git_sha <- NA_character_
  if (include_git && is.character(git_path) && length(git_path) == 1) {
    out <- tryCatch(
      system2("git", c("-C", git_path, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    if (length(out) >= 1 && nzchar(trimws(out[1]))) {
      git_sha <- trimws(out[1])
    }
  }
  list(
    timestamp = timestamp,
    seed = seed,
    scenario = scenario,
    session_info = session_info,
    git_sha = git_sha
  )
}

#' Bundle a result with a reproducibility manifest
#'
#' Combines a result (e.g. a fit from [ild_lme()] or output from
#' [ild_diagnostics()]) with a manifest and optional label for one-shot
#' saving. Typical use: \code{saveRDS(ild_bundle(fit, label = "model_ar1"), "run.rds")}.
#' You can build a manifest with [ild_manifest()] and pass \code{scenario}
#' (e.g. from [ild_summary()]) and \code{seed} before bundling.
#'
#' @param result Any object (e.g. fitted model, diagnostics list).
#' @param manifest List. Reproducibility manifest from [ild_manifest()].
#'   If \code{NULL}, [ild_manifest()] is called with default arguments.
#' @param label Optional character. Short label for the run (e.g.
#'   \code{"model_ar1"} or \code{"diagnostics"}).
#' @return A list with elements \code{result}, \code{manifest}, \code{label},
#'   suitable for [saveRDS()].
#' @examples
#' dat <- ild_prepare(ild_simulate(seed = 1), "id", "time")
#' fit <- ild_lme(y ~ 1 + (1 | id), dat, ar1 = FALSE, warn_no_ar1 = FALSE)
#' b <- ild_bundle(fit, label = "ar1")
#' names(b)
#' b <- ild_bundle(fit, manifest = ild_manifest(seed = 1, scenario = list(n_obs = 50)), label = "run1")
#' @export
ild_bundle <- function(result, manifest = NULL, label = NULL) {
  if (is.null(manifest)) manifest <- ild_manifest()
  list(result = result, manifest = manifest, label = label)
}
