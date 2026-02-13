#' Simulate simple ILD for examples and tests
#'
#' Generates a small tibble with id, time, and one or more variables,
#' optionally with irregular spacing. Use [ild_prepare()] after to get
#' a proper ILD object.
#'
#' @param n_id Integer. Number of persons (default 5).
#' @param n_obs_per Integer. Observations per person (default 10).
#' @param irregular Logical. If `TRUE`, add random jitter to time (default `FALSE`).
#' @param seed Integer. Random seed for reproducibility (default 42).
#' @return A data frame with columns `id`, `time` (POSIXct or numeric), and `y`.
#' @importFrom stats runif rnorm
#' @examples
#' d <- ild_simulate(n_id = 3, n_obs_per = 5, seed = 1)
#' x <- ild_prepare(d, id = "id", time = "time")
#' @export
ild_simulate <- function(n_id = 5L,
                        n_obs_per = 10L,
                        irregular = FALSE,
                        seed = 42L) {
  set.seed(seed)
  n_id <- as.integer(n_id)[1]
  n_obs_per <- as.integer(n_obs_per)[1]
  id <- rep(seq_len(n_id), each = n_obs_per)
  base_time <- rep(seq(0, by = 3600, length.out = n_obs_per), n_id)
  if (irregular) {
    jitter <- runif(length(base_time), -600, 600)
    base_time <- base_time + jitter
    base_time <- pmax(0, base_time)
  }
  time <- as.POSIXct(base_time, origin = "1970-01-01")
  # Simple outcome: person intercept + time effect + noise
  person_eff <- rep(rnorm(n_id, 0, 1), each = n_obs_per)
  time_eff <- base_time / 3600 * 0.1
  y <- person_eff + time_eff + rnorm(n_id * n_obs_per, 0, 0.5)
  data.frame(id = id, time = time, y = y)
}
