# Declarative brms formula templates for simple dynamics (no automatic fit)

#' Template brms formula for person-varying lag slope
#'
#' Returns a suggested formula and notes for fitting with [ild_brms()].
#' This does **not** run the model. Identification, priors, and convergence
#' depend on N, T, and scaling; see \code{vignette("brms-dynamics-recipes", package = "tidyILD")}.
#'
#' @param outcome Character name of the response.
#' @param lag_var Character name of an existing lag column (e.g. from [ild_lag()]).
#' @param id_var Character name of the grouping column in \code{data} (default
#'   \code{"id"} as stored after [ild_prepare()] — use the original id column name).
#' @return A list: \code{formula} (\code{formula} object), \code{notes} (character).
#' @seealso [ild_brms()], [ild_panel_lag_prepare()].
#' @export
ild_brms_dynamics_formula <- function(outcome, lag_var, id_var = "id") {
  outcome <- as.character(outcome)[1L]
  lag_var <- as.character(lag_var)[1L]
  id_var <- as.character(id_var)[1L]
  f <- stats::as.formula(paste0(
    outcome, " ~ ", lag_var, " + (", lag_var, " | ", id_var, ") + (1 | ", id_var, ")"
  ))
  notes <- paste(
    "Random slope for the lag column with random intercept; requires scaling and",
    "informative-ish priors for sd. Not a full VAR; one lagged predictor only."
  )
  list(formula = f, notes = notes)
}
