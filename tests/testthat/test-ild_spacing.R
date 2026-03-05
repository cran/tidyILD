test_that("ild_spacing returns expected elements and recommendation", {
  d <- ild_simulate(n_id = 5, n_obs_per = 10, irregular = TRUE, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = 7200)
  out <- ild_spacing(x)
  expect_named(out, c("median_interval", "iqr", "large_gaps_pct",
    "coefficient_of_variation", "recommendation", "spacing_class"))
  expect_true(is.numeric(out$median_interval))
  expect_true(is.character(out$recommendation))
  expect_true(out$spacing_class %in% c("regular-ish", "irregular-ish"))
  expect_true(grepl("CAR1|AR1", out$recommendation))
})

test_that("ild_spacing with regular-ish data recommends AR1", {
  d <- ild_simulate(n_id = 5, n_obs_per = 10, irregular = FALSE, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time", gap_threshold = Inf)
  out <- ild_spacing(x)
  expect_equal(out$spacing_class, "regular-ish")
  expect_true(grepl("AR1", out$recommendation))
})
