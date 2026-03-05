#' Cross-lag model: lag predictor then fit outcome ~ lag
#'
#' One-call pipeline: [ild_lag()] the predictor, [ild_check_lags()] to audit,
#' then [ild_lme()] to fit outcome on the lagged predictor. Returns the fit,
#' the lag-term coefficient (estimate, CI, p), and lag validity check.
#'
#' @param data An ILD object (see [is_ild()]).
#' @param outcome Character or symbol. Name of the outcome variable (e.g. \code{"mood"}).
#' @param predictor Character or symbol. Name of the predictor to lag (e.g. \code{"stress"}).
#' @param lag Integer. Lag order (default 1).
#' @param mode Character. Passed to [ild_lag()]: \code{"index"}, \code{"gap_aware"} (default), or \code{"time_window"}.
#' @param max_gap Numeric or NULL. Passed to [ild_lag()] when \code{mode = "gap_aware"}.
#' @param ar1 Logical. If \code{TRUE}, fit with [ild_lme()] using AR1/CAR1 (default \code{FALSE}).
#' @param include_diagnostics Logical. If \code{TRUE}, attach [ild_diagnostics()] to the return (default \code{FALSE}).
#' @param ... Passed to [ild_lme()].
#' @return A list: \code{fit} (fitted model), \code{lag_term} (one-row tibble from [tidy_ild_model()] for the lag variable),
#'   \code{lag_check} (tibble from [ild_check_lags()]), \code{data} (ILD with lag column), and optionally \code{diagnostics}.
#' @export
#' @examples
#' d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' out <- ild_crosslag(x, "y", "y", lag = 1, ar1 = FALSE, warn_no_ar1 = FALSE)
#' out$lag_term
#' out$lag_check
ild_crosslag <- function(data,
                         outcome,
                         predictor,
                         lag = 1L,
                         mode = c("gap_aware", "index", "time_window"),
                         max_gap = NULL,
                         ar1 = FALSE,
                         include_diagnostics = FALSE,
                         ...) {
  validate_ild(data)
  mode <- match.arg(mode)
  outcome <- as.character(rlang::ensym(outcome))
  predictor <- as.character(rlang::ensym(predictor))
  if (!outcome %in% names(data)) stop("outcome '", outcome, "' not found in data.", call. = FALSE)
  if (!predictor %in% names(data)) stop("predictor '", predictor, "' not found in data.", call. = FALSE)
  lag <- as.integer(lag)[1L]
  if (lag < 1L) stop("'lag' must be >= 1.", call. = FALSE)
  id_name <- ild_meta(data)$ild_id
  lag_var_name <- paste0(predictor, "_lag", lag)
  data_lagged <- ild_lag(data, dplyr::all_of(predictor), n = lag, mode = mode, max_gap = max_gap)
  lag_check <- ild_check_lags(data_lagged, lag_vars = lag_var_name, max_gap = max_gap)
  f <- stats::as.formula(paste0(outcome, " ~ ", lag_var_name, " + (1|", id_name, ")"))
  dots <- list(...)
  dots$warn_no_ar1 <- NULL
  dots$warn_uncentered <- NULL
  fit <- do.call(ild_lme, c(list(formula = f, data = data_lagged, ar1 = ar1, warn_no_ar1 = FALSE, warn_uncentered = FALSE), dots))
  tidy_tbl <- tidy_ild_model(fit, object = FALSE)
  lag_term <- tidy_tbl[tidy_tbl$term == lag_var_name, ]
  if (nrow(lag_term) == 0) lag_term <- tidy_tbl[0, ]
  out <- list(
    fit = fit,
    lag_term = lag_term,
    lag_check = lag_check,
    data = data_lagged
  )
  if (include_diagnostics) {
    diag_out <- tryCatch(
      ild_diagnostics(fit, type = c("residual_acf", "qq")),
      error = function(e) {
        warning("Could not compute diagnostics: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    out["diagnostics"] <- list(diag_out)
  }
  out
}
