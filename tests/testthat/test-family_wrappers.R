test_that("fit_dst family wrappers invoke fit_dst() properly.", {
  # Binary data
  x <- c(0, 0, 1, 0, 1)
  expect_equal(
    fit_dst_bern(x),
    fit_dst("bern", x)
  )
  # Degenerate data
  x <- rep(4.3, 10)
  expect_equal(
    fit_dst_degenerate(x),
    fit_dst("degenerate", x)
  )
  # Integer data
  x <- 1:10
  expect_equal(
    fit_dst_pois(x),
    fit_dst("pois", x)
  )
  expect_equal(
    fit_dst_geom(x),
    fit_dst("geom", x)
  )
  expect_equal(
    fit_dst_nbinom(x),
    fit_dst("nbinom", x)
  )
  # Positive data
  x <- 1:45 / 100
  expect_equal(
    fit_dst_beta(x),
    fit_dst("beta", x)
  )
  expect_equal(
    fit_dst_cauchy(x),
    fit_dst("cauchy", x)
  )
  expect_equal(
    fit_dst_chisq(x),
    fit_dst("chisq", x)
  )
  expect_equal(
    fit_dst_empirical(x),
    fit_dst("empirical", x)
  )
  expect_equal(
    fit_dst_exp(x),
    fit_dst("exp", x)
  )
  expect_equal(
    fit_dst_f(x),
    fit_dst("f", x)
  )
  expect_equal(
    fit_dst_finite(x),
    fit_dst("finite", x)
  )
  expect_equal(
    fit_dst_gamma(x),
    fit_dst("gamma", x)
  )
  expect_equal(
    fit_dst_gev(x),
    fit_dst("gev", x)
  )
  expect_equal(
    fit_dst_gp(x),
    fit_dst("gp", x)
  )
  expect_equal(
    fit_dst_gumbel(x),
    fit_dst("gumbel", x)
  )
  expect_equal(
    fit_dst_lnorm(x),
    fit_dst("lnorm", x)
  )
  expect_equal(
    fit_dst_lp3(x),
    fit_dst("lp3", x)
  )
  expect_equal(
    fit_dst_norm(x),
    fit_dst("norm", x)
  )
  expect_equal(
    fit_dst_null(x),
    fit_dst("null", x)
  )
  expect_equal(
    fit_dst_t(x),
    fit_dst("t", x)
  )
  expect_equal(
    fit_dst_unif(x),
    fit_dst("unif", x)
  )
  expect_equal(
    fit_dst_weibull(x),
    fit_dst("weibull", x)
  )
  # Pearson3 is a little picky about what data it succeeds on.
  set.seed(1)
  x <- stats::rexp(20)
  expect_equal(
    fit_dst_pearson3(x),
    fit_dst("pearson3", x)
  )
})
