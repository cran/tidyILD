test_that("ild_center adds _bp and _wp columns", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = as.POSIXct(rep(0:2, 2), origin = "1970-01-01"),
    x = c(10, 12, 14, 20, 22, 24)
  )
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_center(x, x)
  expect_true("x_bp" %in% names(out))
  expect_true("x_wp" %in% names(out))
  # Person 1 mean = 12, person 2 mean = 22
  expect_equal(unique(out$x_bp[out$.ild_id == 1]), 12)
  expect_equal(unique(out$x_bp[out$.ild_id == 2]), 22)
  expect_equal(out$x_wp, c(-2, 0, 2, -2, 0, 2))
})

test_that("ild_center preserves ILD attributes", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = 1:4)
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_center(x, x)
  expect_true(is_ild(out))
  expect_equal(attr(out, "ild_n_units"), 2)
})

test_that("ild_center is idempotent for same variable", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = c(1, 2, 3, 4))
  x <- ild_prepare(d, id = "id", time = "time")
  out1 <- ild_center(x, x)
  out2 <- ild_center(out1, x)
  expect_equal(out1$x_bp, out2$x_bp)
  expect_equal(out1$x_wp, out2$x_wp)
})

test_that("ild_center grand_mean adds _gm and _wp_gm", {
  d <- data.frame(id = c(1, 1, 2, 2), time = as.POSIXct(0:3, origin = "1970-01-01"), x = c(1, 2, 3, 4))
  x <- ild_prepare(d, id = "id", time = "time")
  out <- ild_center(x, x, type = "grand_mean")
  expect_true("x_gm" %in% names(out))
  expect_true("x_wp_gm" %in% names(out))
  expect_equal(unique(out$x_gm), 2.5)
})
