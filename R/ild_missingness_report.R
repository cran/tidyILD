#' Missingness workflow report (orchestration)
#'
#' Bundles person-level compliance, pattern summaries, cohort and hazard tables,
#' optional [ild_missing_model()], heuristic flags, and short text snippets for
#' methods sections. This is **diagnostic and reporting** tooling, not a substitute
#' for formal sensitivity analysis or MNAR models.
#'
#' @family ild_diagnostics_utilities
#' @param x An ILD object (see [is_ild()]).
#' @param outcome Character; outcome column with possible `NA`.
#' @param predictors Optional character vector passed to [ild_missing_model()]
#'   when `fit_missing_model` is `TRUE`. If empty, no missingness model is fit.
#' @param fit_missing_model Logical; fit [ild_missing_model()] when predictors
#'   are non-empty.
#' @param random Logical; passed to [ild_missing_model()] (`glmer` vs `glm`).
#' @param expected_occasions Optional integer for [ild_missing_compliance()].
#' @param max_ids,seed Passed to [ild_missing_pattern()] for large cohorts.
#' @param cohort_plot Logical; include ggplot for [ild_missing_cohort()].
#' @return A list (class `ild_missingness_report`) with:
#'   \describe{
#'     \item{compliance}{Tibble from [ild_missing_compliance()].}
#'     \item{pattern}{Output of [ild_missing_pattern()] (includes compliance
#'       columns on `by_id` if `outcome` was passed through).}
#'     \item{cohort}{List from [ild_missing_cohort()].}
#'     \item{hazard}{Tibble from [ild_missing_hazard_first()].}
#'     \item{flags}{Named list: `dropout_late_pooled` (logical from same
#'       heuristic as guardrail `GR_DROPOUT_LATE_CONCENTRATION`).}
#'     \item{missing_model}{Result of [ild_missing_model()] or `NULL`.}
#'     \item{snippets}{Character vector of short paragraphs for copy-paste.}
#'   }
#' @seealso [ild_missing_pattern()], [ild_missing_bias()], [ild_ipw_weights()],
#'   `vignette("ild-missingness-workflow", package = "tidyILD")`
#' @export
ild_missingness_report <- function(x,
                                   outcome,
                                   predictors = NULL,
                                   fit_missing_model = TRUE,
                                   random = FALSE,
                                   expected_occasions = NULL,
                                   max_ids = NULL,
                                   seed = NULL,
                                   cohort_plot = TRUE) {
  validate_ild(x)
  if (!outcome %in% names(x)) {
    stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  }
  predictors <- if (is.null(predictors)) character(0) else unique(as.character(predictors))
  predictors <- predictors[nzchar(predictors)]
  vars <- unique(c(outcome, predictors))
  compliance <- ild_missing_compliance(x, outcome, expected_occasions = expected_occasions)
  pattern <- ild_missing_pattern(
    x,
    vars = vars,
    outcome = outcome,
    expected_occasions = expected_occasions,
    max_ids = max_ids,
    seed = seed
  )
  cohort <- ild_missing_cohort(x, outcome, plot = isTRUE(cohort_plot))
  hazard <- ild_missing_hazard_first(x, outcome)
  flags <- list(dropout_late_pooled = guardrail_dropout_late_heuristic(x, outcome))
  mm <- NULL
  if (isTRUE(fit_missing_model) && length(predictors) > 0L) {
    mm <- tryCatch(
      ild_missing_model(x, outcome, predictors, random = random),
      error = function(e) NULL
    )
  }
  snippets <- .ild_missingness_snippets(
    outcome = outcome,
    predictors = predictors,
    flags = flags,
    missing_model = mm
  )
  structure(
    list(
      compliance = compliance,
      pattern = pattern,
      cohort = cohort,
      hazard = hazard,
      flags = flags,
      missing_model = mm,
      snippets = snippets
    ),
    class = "ild_missingness_report"
  )
}

#' @keywords internal
#' @noRd
.ild_missingness_snippets <- function(outcome, predictors, flags, missing_model) {
  sn <- c(
    overview = paste0(
      "Outcome ", outcome, " was summarized with tidyILD person-level compliance, ",
      "cohort observed fractions by occasion (.ild_seq), and a discrete-time hazard ",
      "of first missing row (ordinal schedule; see ?ild_missing_hazard_first)."
    ),
    mar_cc = paste0(
      "Under MAR on the outcome, mixed models fitted to all available rows use the ",
      "observed-data likelihood for the outcome conditional on random effects; ",
      "this does not address informative missingness (MNAR). Compare complete-case ",
      "subsets to full-data fits as a coarse sensitivity check."
    ),
    mnar = paste0(
      "If missingness may depend on unobserved outcomes or latent states, treat ",
      "logistic missingness models and IPW as sensitivity analyses, not proof of MAR."
    )
  )
  if (isTRUE(flags$dropout_late_pooled)) {
    sn <- c(
      sn,
      dropout_late = paste0(
        "Pooled outcome missingness is higher in the later half of the (sorted) ",
        "timeline than the earlier half (heuristic). Investigate burden, fatigue, ",
        "or selective dropout."
      )
    )
  }
  if (length(predictors) > 0L) {
    sn <- c(
      sn,
      ipw = paste0(
        "After fitting ild_missing_model(), consider ild_ipw_weights() and ",
        "ild_ipw_refit() for inverse-probability weighting as one sensitivity route ",
        "(see vignette and ?ild_ipw_weights)."
      )
    )
  }
  if (!is.null(missing_model) && !is.null(missing_model$message)) {
    sn <- c(sn, missing_model_note = missing_model$message)
  }
  sn
}
