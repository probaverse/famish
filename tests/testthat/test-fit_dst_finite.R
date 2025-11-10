test_that("Fitting a Finite / Empirical distribution has expected behaviour", {
  # Fitting these distributions should be the same for all available methods.
  # Try for four different samples.
  set.seed(1)
  methods <- eval(formals(fit_dst)[["method"]])
  for (i in 1:4) {
    x <- stats::rnorm(10)
    benchmark <- distionary::dst_empirical(x)
    for (method in methods) {
      expect_equal(fit_dst_finite(x, method = method), benchmark)
      expect_equal(fit_dst_empirical(x, method = method), benchmark)
    }
  }
})
