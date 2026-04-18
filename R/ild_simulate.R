#' Simulate simple ILD for examples, tests, and power analysis
#'
#' Generates a tibble with id, time, and outcome \code{y}. Optionally uses
#' AR(1) within-person correlation and configurable WP/BP variance. Use
#' [ild_prepare()] after to get a proper ILD object.
#'
#' @param n_id Integer. Number of persons (default 5).
#' @param n_obs_per Integer. Observations per person (default 10).
#' @param n_time Integer. Alias for \code{n_obs_per} (observations per person). If provided, overrides \code{n_obs_per}.
#' @param irregular Logical. If \code{TRUE}, add random jitter to time (default \code{FALSE}).
#' @param ar1 Numeric or \code{NULL}. If numeric, within-person AR(1) correlation (e.g. 0.4). If \code{NULL} or 0, no AR (default \code{NULL}).
#' @param wp_effect Numeric. Scale (SD) of within-person innovation (default 0.5).
#' @param bp_effect Numeric. Scale (SD) of between-person random intercept (default 1).
#' @param seed Integer. Random seed for reproducibility (default 42).
#' @return A data frame with columns \code{id}, \code{time} (POSIXct), and \code{y}.
#' @seealso \href{../doc/benchmark-simulation-recovery.html}{\code{vignette("benchmark-simulation-recovery", package = "tidyILD")}}
#'   (simulation benchmarks and DGP description).
#' @importFrom stats runif rnorm
#' @examples
#' d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' d2 <- ild_simulate(n_id = 100, n_time = 50, ar1 = 0.4, wp_effect = 0.6,
#'   bp_effect = 0.3, irregular = TRUE, seed = 1)
#' @export
ild_simulate <- function(n_id = 5L,
                        n_obs_per = 10L,
                        n_time = NULL,
                        irregular = FALSE,
                        ar1 = NULL,
                        wp_effect = 0.5,
                        bp_effect = 1,
                        seed = 42L) {
  set.seed(seed)
  n_id <- as.integer(n_id)[1]
  if (!is.null(n_time)) n_obs_per <- as.integer(n_time)[1]
  else n_obs_per <- as.integer(n_obs_per)[1]
  phi <- if (is.null(ar1) || (is.numeric(ar1) && ar1 == 0)) 0 else as.numeric(ar1)[1]
  id <- rep(seq_len(n_id), each = n_obs_per)
  base_time <- rep(seq(0, by = 3600, length.out = n_obs_per), n_id)
  if (irregular) {
    jitter <- runif(length(base_time), -600, 600)
    base_time <- base_time + jitter
    base_time <- pmax(0, base_time)
  }
  time <- as.POSIXct(base_time, origin = "1970-01-01")
  person_eff <- rep(rnorm(n_id, 0, bp_effect), each = n_obs_per)
  time_eff <- base_time / 3600 * 0.1
  n_tot <- n_id * n_obs_per
  if (phi != 0) {
    innov <- rnorm(n_tot, 0, wp_effect)
    y <- numeric(n_tot)
    for (i in seq_len(n_id)) {
      idx <- (i - 1) * n_obs_per + seq_len(n_obs_per)
      y[idx][1] <- person_eff[idx][1] + time_eff[idx][1] + innov[idx][1]
      for (t in 2:n_obs_per) {
        y[idx][t] <- person_eff[idx][t] + time_eff[idx][t] +
          phi * (y[idx][t - 1] - person_eff[idx][t - 1] - time_eff[idx][t - 1]) + innov[idx][t]
      }
    }
  } else {
    y <- person_eff + time_eff + rnorm(n_tot, 0, wp_effect)
  }
  data.frame(id = id, time = time, y = y)
}
