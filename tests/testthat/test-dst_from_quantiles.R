test_that("Distribution matching from quantiles works.", {
  d1 <- distionary::dst_gev(0, 1, 1)
  qf <- distionary::enframe_quantile(d1, at = c(0.1, 0.3, 0.5, 0.65, 0.9),
                                     arg_name = "prob")
  ## Retrieve the GEV parameters
  matching <- dst_from_quantiles(quantile, prob, data = qf, family = "gev")
  expect_equal(distionary::parameters(d1), distionary::parameters(matching),
               tolerance = 1e-2)
  ## Elicit quantiles from expert judgement, and fit a Pearson Type III
  judgement <- data.frame(prob = 1:3 / 4,
                          quantile = c(40, 100, 200))
  fit <- dst_from_quantiles(quantile, prob, data = judgement,
                            family = "pearson3")
  expect_equal(eval_quantile(fit, 1:3 / 4), judgement$quantile,
               tolerance = 1e-2)
})
