# Sequential MSM IPTW (ild_iptw_msm_weights)

test_that("ild_iptw_msm_weights cumulative weight matches manual cumprod", {
  set.seed(1201)
  d <- ild_simulate(n_id = 8, n_obs_per = 5, seed = 1201)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  d <- ild_prepare(d, id = "id", time = "time")
  x <- ild_iptw_msm_weights(d, treatment = "trt", history = ~ stress,
    stabilize = "marginal", trim = numeric(0))
  fits <- attr(x, "ild_iptw_msm_fits")
  expect_true(length(fits) >= 1L)
  n <- nrow(x)
  r <- rep(NA_real_, n)
  seqv <- x[[".ild_seq"]]
  A <- x[["trt"]]
  if (is.factor(A)) A <- as.integer(A) - 1L
  A <- as.numeric(A)
  for (nm in names(fits)) {
    t <- as.numeric(nm)
    idx <- which(as.numeric(seqv) == t & !is.na(A))
    fit <- fits[[nm]]
    p_hat <- stats::predict(fit, newdata = as.data.frame(x[idx, , drop = FALSE]), type = "response")
    p_hat <- pmax(pmin(p_hat, 1 - 1e-6), 1e-6)
    Ai <- A[idx]
    p1 <- mean(Ai, na.rm = TRUE)
    p1 <- pmax(pmin(p1, 1 - 1e-6), 1e-6)
    r[idx] <- ifelse(Ai >= 0.5, p1 / p_hat, (1 - p1) / (1 - p_hat))
  }
  id <- x[[".ild_id"]]
  ord <- order(id, seqv, seq_len(n))
  ido <- id[ord]
  r_ord <- r[ord]
  r_ord[is.na(r_ord)] <- 1
  w_ord <- rep(NA_real_, n)
  i <- 1L
  while (i <= n) {
    j <- i
    while (j < n && ido[j + 1L] == ido[i]) j <- j + 1L
    idx <- i:j
    w_ord[idx] <- cumprod(r_ord[idx])
    i <- j + 1L
  }
  w <- rep(NA_real_, n)
  w[ord] <- w_ord
  expect_equal(x$.ipw_treat, w, tolerance = 1e-5)
})

test_that("ild_iptw_msm_weights + joint + refit + diagnose (smoke)", {
  set.seed(1202)
  d <- ild_simulate(n_id = 15, n_obs_per = 6, seed = 1202)
  d$stress <- rnorm(nrow(d))
  d$trt <- as.integer(stats::rbinom(nrow(d), 1L, 0.45))
  d <- ild_prepare(d, id = "id", time = "time")
  d <- ild_center(d, y)
  d <- ild_lag(d, stress, mode = "gap_aware", max_gap = Inf)
  d <- ild_lag(d, trt, mode = "gap_aware", max_gap = Inf)
  x <- ild_iptw_msm_weights(d, "trt", history = ~ stress_lag1 + trt_lag1)
  x <- ild_ipcw_weights(x, predictors = "stress")
  x <- ild_joint_msm_weights(x)
  fit <- ild_lme(y ~ y_bp + y_wp + stress + (1 | id), data = x,
    ar1 = FALSE, warn_no_ar1 = FALSE, warn_uncentered = FALSE)
  fitw <- ild_ipw_refit(fit, data = x)
  expect_true(inherits(fitw, "lmerMod"))
  b <- ild_diagnose(fitw, data = x, type = "qq", causal_detail = TRUE)
  expect_true(!is.null(b$causal$msm_treat_weight_by_occasion))
})
