test_that("Odd inputs", {
  # Family missing leads to an error, even if x has length 0.
  expect_error(fit_dst(x = 1:10))
  expect_error(fit_dst(x = numeric(0)))
  # `x` with length 0 has the same behaviour as not enough data to fit dst.
  expect_equal(suppressWarnings(fit_dst("norm", x = numeric(0))), dnull)
})


test_that("Some unsupported combinations throw a warning", {
  # For these examples, we also know that they will fail to fit,
  # so we can also test their return value.
  # Note that all sorts of error and warning messages may show up here even
  # after the anticipated built-in warning; suppress these, as they are not
  # important.
  dnull <- distionary::dst_null()
  suppressWarnings(expect_warning(fit_dst("rockface", 1:10)))
  suppressWarnings(expect_warning(fit_dst("binom", 1:10)))
  suppressWarnings(expect_warning(fit_dst("hyper", 1:10)))
  suppressWarnings(expect_equal(suppressWarnings(fit_dst("rockface", 1:10)), dnull))
  suppressWarnings(expect_equal(suppressWarnings(fit_dst("binom", 1:10)), dnull))
  suppressWarnings(expect_equal(suppressWarnings(fit_dst("hyper", 1:10)), dnull))
})
