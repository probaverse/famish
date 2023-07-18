test_that("GEV parameters are realistic: MLE", {
  d <- distionary::dst_gev(-3, 5, 2)
  set.seed(4)
  x <- distionary::realise(d, n = 1000)
  mle <- fit_dst_gev(x, method = "mle")
  delta <- abs(unlist(parameters(d)) - unlist(parameters(mle)))
  expect_true(all(delta < 1))
})

test_that("GEV parameters are realistic: l-moments", {
  d <- distionary::dst_gev(3, 6, -2)
  set.seed(4)
  x <- distionary::realise(d, n = 1000)
  lmom <- fit_dst_gev(x, method = "lmom")
  delta <- abs(unlist(parameters(d)) - unlist(parameters(lmom)))
  expect_true(all(delta < 1))
})
