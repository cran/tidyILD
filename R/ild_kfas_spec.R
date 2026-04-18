# Internal state-space specification vocabulary for ild_kfas() (KFAS backend)

#' @keywords internal
ILD_KFAS_STATE_SPECS <- c(
  "local_level",
  "local_trend",
  "ar1_state",
  "regression_local_level",
  "regression_ar1_state"
)

ILD_KFAS_SCHEMA_VERSION <- "0.1"

#' Validate and normalize state_spec for ild_kfas
#' @keywords internal
#' @noRd
ild_kfas_normalize_state_spec <- function(state_spec, implemented_only = TRUE) {
  ss <- state_spec[1L]
  if (!ss %in% ILD_KFAS_STATE_SPECS) {
    stop(
      "state_spec must be one of: ",
      paste(ILD_KFAS_STATE_SPECS, collapse = ", "),
      call. = FALSE
    )
  }
  if (isTRUE(implemented_only) && !ss %in% c("local_level")) {
    stop(
      "state_spec = '", ss, "' is not implemented yet. Currently supported: local_level.",
      call. = FALSE
    )
  }
  ss
}

#' Build internal spec list for provenance and downstream methods
#' @keywords internal
#' @noRd
ild_kfas_internal_spec <- function(state_spec,
                                   observation_family,
                                   time_units,
                                   irregular_time,
                                   fit_context = "single_series",
                                   pool_mode = "none") {
  list(
    state_spec = state_spec,
    observation_family = observation_family,
    time_units = time_units,
    irregular_time = irregular_time,
    fit_context = fit_context,
    pool_mode = pool_mode,
    schema_version = ILD_KFAS_SCHEMA_VERSION
  )
}
