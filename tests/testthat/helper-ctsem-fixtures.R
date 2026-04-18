ctsem_fixture_data <- function(seed = 9401L, n_id = 1L, n_obs_per = 60L) {
  d <- ild_simulate(n_id = n_id, n_obs_per = n_obs_per, seed = seed)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  x
}
