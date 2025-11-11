test_that("Odd inputs", {
  # Family missing leads to an error, even if x has length 0.
  expect_error(fit_dst(x = 1:10))
  expect_error(fit_dst(x = numeric(0)))
  # `x` with length 0 has the same behaviour as not enough data to fit dst.
  dnull <- distionary::dst_null()
  expect_equal(suppressWarnings(fit_dst("norm", x = numeric(0))), dnull)
})

test_that("Violated inputs", {
  x <- 1:10
  f <- "norm"
  # First two arguments are needed.
  expect_error(fit_dst())
  expect_error(fit_dst(family = f))
  expect_error(fit_dst(x = x))
  # Type checks.
  expect_error(fit_dst(family = 1, x = x))
  expect_error(fit_dst(family = f, x = "not numeric"))
  # Unsupported argument specifications are not allowed.
  expect_error(fit_dst(f, x, method = "my_method"))
  expect_error(fit_dst(f, x, na_action = "my_na_action"))
  expect_error(fit_dst(f, x, on_unres = "my_on_unres"))
})

test_that("Failure handling", {
  dnull <- distionary::dst_null()
  ## NA handling in data
  expect_equal(
    fit_dst("exp", c(1:5, NA), na_action = "null"),
    dnull
  )
  expect_equal(
    fit_dst("exp", c(1:5, NA), na_action = "drop"),
    fit_dst("exp", 1:5)
  )
  expect_error(
    fit_dst("exp", c(1:5, NA), na_action = "fail")
  )
  ## On unresolved fitting
  expect_equal(
    suppressWarnings(fit_dst("exp", -(1:5), on_unres = "null")),
    dnull
  )
  expect_error(
    fit_dst("exp", -(1:5), on_unres = "fail")
  )
  # Missing data action takes precedence over unresolved handling.
  expect_equal(
    fit_dst("exp", c(-1, -2, NA), na_action = "null", on_unres = "fail"),
    dnull
  )
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
  suppressWarnings(expect_equal(
    suppressWarnings(fit_dst("rockface", 1:10)),
    dnull
  ))
  suppressWarnings(expect_equal(
    suppressWarnings(fit_dst("binom", 1:10)),
    dnull
  ))
  suppressWarnings(expect_equal(
    suppressWarnings(fit_dst("hyper", 1:10)),
    dnull
  ))
})
