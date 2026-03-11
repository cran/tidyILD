test_that("ild_prepare then ild_center yields two provenance steps", {
  d <- data.frame(
    id = c(1, 1, 2, 2),
    time = as.POSIXct(c(0, 10, 0, 10), origin = "1970-01-01"),
    x = c(10, 12, 20, 22)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, x)
  prov <- ild_provenance(x)
  expect_false(is.null(prov))
  expect_equal(length(prov$steps), 2L)
  expect_equal(prov$steps[[1]]$step, "ild_prepare")
  expect_equal(prov$steps[[2]]$step, "ild_center")
})

test_that("ild_provenance(x) has version and steps with step/timestamp/args/outputs", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  prov <- ild_provenance(x)
  expect_false(is.null(prov))
  expect_true(is.character(prov$version))
  expect_true(nchar(prov$version) >= 1L)
  expect_true(is.list(prov$steps))
  expect_equal(length(prov$steps), 1L)
  s1 <- prov$steps[[1]]
  expect_equal(s1$step, "ild_prepare")
  expect_true("timestamp" %in% names(s1))
  expect_true(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$", s1$timestamp))
  expect_true(is.list(s1$args))
  expect_true("id" %in% names(s1$args))
  expect_true("time" %in% names(s1$args))
  expect_true(is.list(s1$outputs))
  expect_true("n_id" %in% names(s1$outputs))
  expect_true("n_obs" %in% names(s1$outputs))
})

test_that("ild_history(x) runs without error and prints step names", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, x)
  prov <- ild_history(x)
  expect_false(is.null(prov))
  expect_equal(length(prov$steps), 2L)
  expect_output(ild_history(x), "ild_prepare")
  expect_output(ild_history(x), "ild_center")
})

test_that("ild_history(x) returns message when no provenance", {
  d <- data.frame(id = c(1, 1), time = as.POSIXct(0:1, origin = "1970-01-01"), x = 1:2)
  x <- ild_prepare(d, id = "id", time = "time")
  # Object has provenance from ild_prepare; test ild_provenance on NULL-like is via as_ild
  # For "no provenance" we'd need a legacy object without provenance - ild_normalize adds empty steps now.
  # So instead: ild_provenance returns NULL for non-ILD
  expect_null(ild_provenance(data.frame(a = 1)))
  expect_message(ild_history(data.frame(a = 1)), "No provenance")
})

test_that("analysis objects have ild_provenance and ild_provenance() returns it", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  x <- ild_center(x, y)
  fit <- ild_lme(y ~ y_bp + y_wp + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  ap <- ild_provenance(fit)
  expect_false(is.null(ap))
  expect_true(is.list(ap$analysis_steps))
  expect_equal(length(ap$analysis_steps), 1L)
  expect_equal(ap$analysis_steps[[1]]$step, "ild_lme")
  expect_false(is.null(ap$source_data_provenance))
  expect_equal(length(ap$source_data_provenance$steps), 2L)
  expect_output(ild_history(fit), "ild_lme")
})

test_that("ild_missing_model return has analysis provenance, data provenance unchanged", {
  set.seed(1)
  d <- ild_simulate(n_id = 5, n_obs_per = 6, seed = 1)
  d$stress <- rnorm(nrow(d))
  d$mood <- d$y
  d$mood[sample(nrow(d), 5)] <- NA
  x <- ild_prepare(d, id = "id", time = "time")
  data_steps_before <- length(ild_provenance(x)$steps)
  mm <- ild_missing_model(x, "mood", "stress")
  expect_false(is.null(attr(mm, "ild_provenance")))
  expect_equal(attr(mm, "ild_provenance")$analysis_steps[[1]]$step, "ild_missing_model")
  expect_equal(length(ild_provenance(x)$steps), data_steps_before)
})

test_that("data provenance has object_type and steps have step_id", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  prov <- ild_provenance(x)
  expect_equal(prov$object_type, "ild_data")
  expect_equal(prov$steps[[1]]$step_id, "1")
  x <- ild_center(x, x)
  prov <- ild_provenance(x)
  expect_equal(prov$steps[[2]]$step_id, "2")
})

test_that("analysis provenance has object_type and analysis_steps have step_id", {
  set.seed(1)
  d <- ild_simulate(n_id = 4, n_obs_per = 5, seed = 1)
  x <- ild_prepare(d, id = "id", time = "time")
  fit <- ild_lme(y ~ 1 + (1 | id), data = x, ar1 = FALSE, warn_no_ar1 = FALSE)
  ap <- ild_provenance(fit)
  expect_equal(ap$object_type, "ild_model")
  expect_equal(ap$analysis_steps[[1]]$step_id, "1")
})
