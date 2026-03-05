test_that("ild_decomposition returns table with wp_var, bp_var, ratio", {
  d <- ild_simulate(n_id = 10, n_obs_per = 8, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_decomposition(x, "y")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("variable", "wp_var", "bp_var", "ratio"))
  expect_equal(out$variable, "y")
  expect_true(is.numeric(out$wp_var) && is.numeric(out$bp_var) && is.numeric(out$ratio))
  expect_true(out$wp_var >= 0 && out$bp_var >= 0)
})

test_that("ild_decomposition with plot returns list with table and plot", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_decomposition(x, "y", plot = TRUE)
  expect_true(is.list(out))
  expect_named(out, c("table", "plot"))
  expect_s3_class(out$table, "tbl_df")
  expect_s3_class(out$plot, "gg")
})

test_that("ild_decomposition errors on non-numeric variable", {
  d <- ild_simulate(n_id = 3, n_obs_per = 4, seed = 1)
  d$cat <- rep(c("a", "b"), length.out = nrow(d))
  x <- ild_prepare(d, id = "id", time = "time")
  expect_error(ild_decomposition(x, "cat"), "not numeric")
})

test_that("ild_center_plot returns ggplot matching decomposition plot", {
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  p <- ild_center_plot(x, "y")
  expect_s3_class(p, "gg")
  p2 <- ild_decomposition(x, "y", plot = TRUE)$plot
  expect_s3_class(p2, "gg")
})
