test_that("Distribution matching from quantiles works.", {
  d1 <- distionary::dst_gev(0, 1, 1)
  qf <- distionary::enframe_quantile(d1, at = c(0.1, 0.5, 0.65),
                                     arg_name = "prob")
  ## Retrieve the GEV parameters
  matching <- dst_from_quantiles(quantile, prob, data = qf, family = "gev")
  expect_equal(distionary::parameters(d1), distionary::parameters(matching),
               tolerance = 1e-3)
  ## Elicit quantiles from expert judgement, and fit a Pearson Type III
  judgement <- data.frame(prob = c(0.25, 0.5, 0.75),
                          quantile = c(40, 100, 200))
  fit <- dst_from_quantiles(quantile, prob, data = judgement,
                            family = "pearson3")
  mu <- mean(fit)
  sigma <- stdev(fit)
  expect_true(70 < mu && mu < 150)
  expect_true(30 < sigma && sigma < 100)
})
