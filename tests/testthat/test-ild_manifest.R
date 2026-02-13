test_that("ild_manifest returns list with expected names", {
  m <- ild_manifest()
  expect_type(m, "list")
  expect_named(m, c("timestamp", "seed", "scenario", "session_info", "git_sha"))
  expect_s3_class(m$timestamp, "POSIXct")
  expect_true(difftime(Sys.time(), m$timestamp, units = "secs") < 60)
  expect_null(m$seed)
  expect_null(m$scenario)
  expect_type(m$session_info, "list")
  expect_length(m$git_sha, 1)
})

test_that("ild_manifest include_session FALSE gives NULL session_info", {
  m <- ild_manifest(include_session = FALSE)
  expect_null(m$session_info)
})

test_that("ild_manifest stores seed and scenario", {
  m <- ild_manifest(seed = 42, scenario = list(a = 1, n_obs = 100))
  expect_equal(m$seed, 42)
  expect_equal(m$scenario, list(a = 1, n_obs = 100))
})

test_that("ild_manifest include_git returns character or NA", {
  m <- ild_manifest(include_git = TRUE)
  expect_length(m$git_sha, 1)
  expect_true(is.character(m$git_sha))
  expect_true(is.na(m$git_sha) || nzchar(m$git_sha))
})

test_that("ild_bundle returns result, manifest, label", {
  b <- ild_bundle(1 + 1, label = "x")
  expect_named(b, c("result", "manifest", "label"))
  expect_equal(b$result, 2)
  expect_equal(b$label, "x")
  expect_named(b$manifest, c("timestamp", "seed", "scenario", "session_info", "git_sha"))
})

test_that("ild_bundle with NULL manifest calls ild_manifest()", {
  b <- ild_bundle("fit", manifest = NULL)
  expect_equal(b$result, "fit")
  expect_null(b$label)
  expect_type(b$manifest, "list")
  expect_true("timestamp" %in% names(b$manifest))
})

test_that("ild_bundle preserves user-supplied manifest and label", {
  m <- ild_manifest(seed = 1, scenario = list(formula = "y ~ x"), include_session = FALSE)
  b <- ild_bundle("result", manifest = m, label = "run1")
  expect_equal(b$manifest$seed, 1)
  expect_equal(b$manifest$scenario$formula, "y ~ x")
  expect_equal(b$label, "run1")
})

test_that("ild_bundle round-trip via saveRDS preserves structure", {
  skip_on_cran()
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf, force = TRUE))
  b <- ild_bundle(1 + 1, manifest = ild_manifest(include_session = FALSE), label = "x")
  saveRDS(b, tf)
  b2 <- readRDS(tf)
  expect_named(b2, c("result", "manifest", "label"))
  expect_equal(b2$result, b$result)
  expect_equal(b2$label, b$label)
  expect_named(b2$manifest, names(b$manifest))
})
