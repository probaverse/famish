lp3_lmom_sample <- function(skew, meanlog = 3.04, sdlog = 0.4, n = 60) {
  probs <- seq(0.02, 0.98, length.out = n)
  exp(lmom::quape3(
    probs,
    c(mu = meanlog, sigma = sdlog, gamma = skew)
  ))
}

test_that("lp3 lmom-log matches lmom for negative log-skew", {
  x <- lp3_lmom_sample(skew = -0.93)
  lmom_params <- lmom::pelpe3(lmom::samlmu(log(x)))
  fit <- fit_dst("lp3", x, method = "lmom-log")
  fit_params <- distionary::parameters(fit)
  
  expect_lt(lmom_params[["gamma"]], 0)
  expect_equal(fit_params[["meanlog"]], unname(lmom_params[["mu"]]))
  expect_equal(fit_params[["sdlog"]], unname(lmom_params[["sigma"]]))
  expect_equal(fit_params[["skew"]], unname(lmom_params[["gamma"]]))
})

test_that("lp3 lmom-log preserves the positive-skew branch", {
  x <- lp3_lmom_sample(skew = 0.8)
  lmom_params <- lmom::pelpe3(lmom::samlmu(log(x)))
  fit <- fit_dst("lp3", x, method = "lmom-log")
  fit_params <- distionary::parameters(fit)
  theta <- distionary::parameters(wrapper_lmom("pearson3", log(x)))
  probs <- c(0.1, 0.5, 0.9, 0.99)
  
  expect_gt(lmom_params[["gamma"]], 0)
  expect_equal(fit_params[["meanlog"]], unname(lmom_params[["mu"]]))
  expect_equal(fit_params[["sdlog"]], unname(lmom_params[["sigma"]]))
  expect_equal(fit_params[["skew"]], unname(lmom_params[["gamma"]]))
  expect_equal(
    theta[["location"]] + theta[["scale"]] * theta[["shape"]],
    unname(lmom_params[["mu"]])
  )
  expect_equal(
    distionary::eval_quantile(fit, probs),
    exp(lmom::quape3(probs, lmom_params))
  )
  expect_equal(
    distionary::eval_cdf(
      fit,
      exp(lmom::quape3(probs, lmom_params))
    ),
    probs
  )
})

test_that("lp3 lmom-log maps pelpe3 output directly to dst_lp3 parameters", {
  x_neg <- lp3_lmom_sample(skew = -0.93)
  p_neg <- lmom::pelpe3(lmom::samlmu(log(x_neg)))
  fit_neg <- fit_dst("lp3", x_neg, method = "lmom-log")
  x_pos <- lp3_lmom_sample(skew = 0.8)
  p_pos <- lmom::pelpe3(lmom::samlmu(log(x_pos)))
  fit_pos <- fit_dst("lp3", x_pos, method = "lmom-log")
  
  expect_equal(
    distionary::parameters(fit_neg),
    list(
      meanlog = unname(p_neg[["mu"]]),
      sdlog = unname(p_neg[["sigma"]]),
      skew = unname(p_neg[["gamma"]])
    )
  )
  expect_equal(
    distionary::parameters(fit_pos),
    list(
      meanlog = unname(p_pos[["mu"]]),
      sdlog = unname(p_pos[["sigma"]]),
      skew = unname(p_pos[["gamma"]])
    )
  )
})

test_that("pearson3 mapping only uses location + scale * shape for positive skew", {
  x_neg <- lp3_lmom_sample(skew = -0.93)
  p_neg <- lmom::pelpe3(lmom::samlmu(log(x_neg)))
  theta_neg <- distionary::parameters(wrapper_lmom("pearson3", log(x_neg)))
  x_pos <- lp3_lmom_sample(skew = 0.8)
  p_pos <- lmom::pelpe3(lmom::samlmu(log(x_pos)))
  theta_pos <- distionary::parameters(wrapper_lmom("pearson3", log(x_pos)))
  
  expect_lt(p_neg[["gamma"]], 0)
  expect_gt(p_pos[["gamma"]], 0)
  expect_equal(
    theta_neg[["location"]] - theta_neg[["scale"]] * theta_neg[["shape"]],
    unname(p_neg[["mu"]])
  )
  expect_false(isTRUE(all.equal(
    theta_neg[["location"]] + theta_neg[["scale"]] * theta_neg[["shape"]],
    unname(p_neg[["mu"]])
  )))
  expect_equal(
    theta_pos[["location"]] + theta_pos[["scale"]] * theta_pos[["shape"]],
    unname(p_pos[["mu"]])
  )
})
