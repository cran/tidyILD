test_that("ild_methods returns non-empty string for data object", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  out <- ild_methods(x)
  expect_type(out, "character")
  expect_true(nchar(out) > 0)
  expect_true(grepl("ild_prepare", out))
  expect_true(grepl("ild_center", out))
})

test_that("ild_methods returns non-empty string for model object", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  out <- ild_methods(fit)
  expect_type(out, "character")
  expect_true(nchar(out) > 0)
  expect_true(grepl("ild_lme", out))
})

test_that("ild_methods returns message when no provenance", {
  out <- ild_methods(data.frame(a = 1))
  expect_equal(out, "No provenance recorded for this object.")
})

test_that("ild_compare_pipelines returns structure and detects match", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x1 <- ild_prepare(d, id = "id", time = "time")
  x1 <- ild_center(x1, y)
  x2 <- ild_prepare(d, id = "id", time = "time")
  x2 <- ild_center(x2, y)
  cmp <- ild_compare_pipelines(x1, x2)
  expect_s3_class(cmp, "ild_compare_pipelines")
  expect_true(is.character(cmp$summary))
  expect_true(length(cmp$only_in_first) >= 0)
  expect_true(length(cmp$only_in_second) >= 0)
})

test_that("ild_compare_pipelines detects different gap_threshold", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x1 <- ild_prepare(d, id = "id", time = "time", gap_threshold = 10)
  x2 <- ild_prepare(d, id = "id", time = "time", gap_threshold = 20)
  cmp <- ild_compare_pipelines(x1, x2)
  expect_true(length(cmp$differing) > 0 || any(grepl("gap", cmp$summary, ignore.case = TRUE)))
})

test_that("ild_export_provenance writes JSON file when jsonlite available", {
  skip_if_not_installed("jsonlite")
  set.seed(1)
  d <- ild_simulate(n_id = 3, n_obs_per = 4, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  path <- tempfile(fileext = ".json")
  ild_export_provenance(x, path)
  expect_true(file.exists(path))
  txt <- readLines(path)
  expect_true(any(grepl("ild_prepare", txt)))
  expect_true(any(grepl("object_type", txt)))
})

test_that("ild_export_provenance errors when no provenance", {
  expect_error(ild_export_provenance(data.frame(a = 1), tempfile(fileext = ".json")), "no provenance")
})

test_that("ild_export_provenance errors when path missing", {
  set.seed(1)
  x <- ild_prepare(ild_simulate(n_id = 2, n_obs_per = 3, seed = 1), id = "id", time = "time")
  expect_error(ild_export_provenance(x), "path")
})
