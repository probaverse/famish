families <- c(
  dst_weibull,
  dst_unif,
  dst_t,
  dst_pois,
  dst_pearson3,
  dst_norm,
  dst_nbinom,
  dst_lp3,
  dst_lnorm,
  dst_hyper,
  dst_gp,
  dst_gev,
  dst_geom,
  dst_gamma,
  dst_f,
  dst_exp,
  dst_chisq,
  dst_cauchy,
  dst_binom,
  dst_beta,
  dst_bern,
  dst_finite,
  dst_empirical,
  dst_degenerate,
  
)

test_that("Odd inputs", {
  # Family missing leads to an error, even if x has length 0.
  expect_error(fit_dst(x = 1:10))
  expect_error(fit_dst(x = numeric(0)))
  # `x` with length 0 has the same behaviour as not enough data to fit dst.
  expect_equal(fit_dst())
})

test_that("Data outside of distribution's range leads to Null distribution", {
  # From the perspective of narrowing down a space of distributions to match
  # some criteria, trying to fit a distribution to data that is outside of its
  # range narrows the space to an empty set, because no distributions satisfy
  # the criteria.
  for (fam in families) {
    dst <- fam()
    # Generate data outside of the distribution's range.
    if (dst$variable == "continuous") {
      x <- c(dst$min - 10, dst$max + 10)
    } else {
      x <- c(dst$min - 10, dst$max + 10)
    }
    fit <- fit_dst(x, name = dst$name, method = "mle")
    expect_true(inherits(fit, "dst_null"))
  }
})