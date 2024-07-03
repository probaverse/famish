test_that("Distribution matching from quantiles works.", {
  library(distionary)
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

test_that("For Battle River quantiles", {
  RPs <- c(2, 5, 10, 20, 25, 50, 100, 200, 500)
  flows <- c(32, 58, 81, 110, 120, 150, 200, 250, 340)
  gev <- dst_from_quantiles(flows, 1 - 1 / RPs, family = "gev")
  # Visual assessment
  rpgrid <- exp(seq(log(2), log(500), length.out = 100))
  quantiles <- eval_return(gev, at = rpgrid)
  plot(log(RPs), log(flows))
  lines(log(rpgrid), log(quantiles))
  # Formal assessment
  flows_hat <- eval_return(gev, at = RPs)
  expect_equal(log(flows), log(flows_hat), tolerance = 0.01)
})
