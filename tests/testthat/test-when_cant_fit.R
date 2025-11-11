test_that("Behaviour during known failures is expected", {
  available <- available_methods()
  set.seed(42)
  x <- -10:0
  dnull <- distionary::dst_null()
  test_families <- c(
    "bern", "beta", "chisq", "degenerate", "exp", "f", "gamma", "geom", "gp",
    "lnorm", "lp3", "nbinom", "pois", "weibull"
  )
  for (fam in test_families) {
    for (method in intersect(available[[fam]], c("mle", "mme", "lmom"))) {
      expect_equal(
        suppressWarnings(fit_dst(fam, x, method = method)),
        dnull
      )
      expect_error(
        suppressWarnings(fit_dst(fam, x, method = method, on_unres = "fail"))
      )
    }
  }
})

test_that("Cauchy by moments never fits", {
  # (Because its moments do not exist)
  expect_error(suppressWarnings(
    fit_dst("cauchy", -10:10, method = "mme", on_unres = "fail")
  ))
  expect_error(suppressWarnings(
    fit_dst("cauchy", -10:10, method = "lmom", on_unres = "fail")
  ))
  dnull <- distionary::dst_null()
  expect_warning(fit_dst("cauchy", -10:10, method = "mme"))
  expect_warning(fit_dst("cauchy", -10:10, method = "lmom"))
  expect_equal(
    suppressWarnings(fit_dst("cauchy", -10:10, method = "mme")),
    dnull
  )
  expect_equal(
    suppressWarnings(fit_dst("cauchy", -10:10, method = "lmom")),
    dnull
  )
})
