test_that("GPD parameters are realistic.", {
  library(distionary)
  d <- distionary::dst_gpd(3, 10, -2)
  set.seed(24)
  x <- distionary::realise(d, n = 1000)
  mle <- suppressWarnings(fit_dst_gpd(x, threshold = min(x) - 1e-6,
                                      method = "mle"))
  lmom <- fit_dst_gpd(x, method = "lmom")
  delta <- abs(unlist(parameters(d)) - unlist(parameters(mle)))
  expect_true(all(delta < 1))
  delta <- abs(unlist(parameters(d)) - unlist(parameters(lmom)))
  expect_true(all(delta < 1))
})
