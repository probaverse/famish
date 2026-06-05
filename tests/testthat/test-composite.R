# Tests for composite quantile (CQE) and expectile (CEE) estimation.

test_that("cqe() and cee() build estimator objects", {
  e <- cqe()
  expect_s3_class(e, "famish_estimator")
  expect_s3_class(e, "cqe")
  expect_equal(e$tau_cutoff, 0)
  expect_equal(e$representation, "quantile")
  expect_equal(cee()$representation, "expectile")
  expect_equal(cqe(tau_cutoff = 0.8)$tau_cutoff, 0.8)
  expect_output(print(cqe()), "Composite Quantile")
  expect_output(print(cee()), "Composite Expectile")
})

test_that("Estimator arguments are validated", {
  expect_error(cqe(tau_cutoff = 1.5))
  expect_error(cqe(tau_cutoff = -0.1))
  expect_error(cee(weight = "not a function"))
})

test_that("The string method shortcut equals the default estimator", {
  set.seed(1)
  x <- rgamma(200, shape = 2, rate = 1)
  expect_equal(
    distionary::parameters(fit_dst("gamma", x, method = "cqe")),
    distionary::parameters(fit_dst("gamma", x, method = cqe()))
  )
  expect_equal(
    distionary::parameters(fit_dst("exp", x, method = "cee")),
    distionary::parameters(fit_dst("exp", x, method = cee()))
  )
})

test_that("CQE recovers parameters for continuous families", {
  set.seed(2)
  cases <- list(
    list(d = distionary::dst_norm(3, 2), fam = "norm", tol = 0.4),
    list(d = distionary::dst_gamma(2, 1), fam = "gamma", tol = 0.5),
    list(d = distionary::dst_exp(2), fam = "exp", tol = 0.4)
  )
  for (case in cases) {
    x <- distionary::realise(case$d, 1000)
    fit <- fit_dst(case$fam, x, method = cqe())
    expect_true(distionary::is_distribution(fit))
    expect_equal(
      unlist(distionary::parameters(fit)),
      unlist(distionary::parameters(case$d)),
      tolerance = case$tol, ignore_attr = TRUE
    )
  }
})

test_that("CEE recovers parameters (analytic-expectile family)", {
  set.seed(3)
  x <- distionary::realise(distionary::dst_exp(2), 1000)
  fit <- fit_dst("exp", x, method = cee())
  expect_true(distionary::is_distribution(fit))
  expect_equal(
    unlist(distionary::parameters(fit)), c(rate = 2),
    tolerance = 0.4, ignore_attr = TRUE
  )
})

test_that("CEE returns a distribution for a network-expectile family", {
  set.seed(4)
  x <- distionary::realise(distionary::dst_norm(0, 1), 200)
  fit <- fit_dst("norm", x, method = cee())
  expect_true(distionary::is_distribution(fit))
  expect_equal(distionary::vtype(fit), "continuous")
})

test_that("The tail cutoff changes the estimate", {
  set.seed(5)
  x <- distionary::realise(distionary::dst_gev(20, 8, 0.1), 300)
  full <- unlist(distionary::parameters(fit_dst("gev", x, method = cqe(0))))
  tail <- unlist(distionary::parameters(fit_dst("gev", x, method = cqe(0.9))))
  expect_false(isTRUE(all.equal(full, tail)))
})

test_that("Family wrappers accept composite estimators and shortcuts", {
  set.seed(6)
  x <- distionary::realise(distionary::dst_norm(0, 1), 200)
  expect_true(distionary::is_distribution(fit_dst_norm(x, method = cqe())))
  expect_equal(
    distionary::parameters(fit_dst_norm(x, method = "cqe")),
    distionary::parameters(fit_dst_norm(x, method = cqe()))
  )
  expect_true(
    distionary::is_distribution(fit_dst_exp(rexp(200, 2), method = cee()))
  )
  # Built-in method validation in the wrappers still holds.
  expect_error(fit_dst_norm(x, method = "nonsense"))
  expect_error(fit_dst_gev(x, method = "mme"))
})

test_that("CEE on a discrete family fails gracefully", {
  x <- c(1, 2, 3, 2, 1, 4, 2)
  expect_warning(
    res <- fit_dst("pois", x, method = cee()),
    "Null distribution"
  )
  expect_equal(res, distionary::dst_null())
  expect_error(fit_dst("pois", x, method = cee(), on_unres = "fail"))
})
