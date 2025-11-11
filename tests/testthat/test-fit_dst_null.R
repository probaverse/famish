test_that("Fitting a Null distribution has expected behaviour", {
  dnull <- distionary::dst_null()
  expect_equal(fit_dst_null(1:10), dnull)
  expect_equal(fit_dst("null", 1:10, on_unres = "fail"), dnull)
  expect_error(fit_dst("null", NA, na_action = "fail"))
  expect_equal(fit_dst("null", numeric(), na_action = "fail"), dnull)
  methods <- eval(formals(fit_dst)[["method"]])
  for (method in methods) {
    expect_equal(fit_dst_null(1, method = method), dnull)
  }
})
