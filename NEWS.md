# famish (development version)

* `fit_dst()` no longer loads the testthat namespace. Internal argument
  validation in `supported_combination()` used `checkmate::expect_character()`,
  a testthat-backed expectation, rather than `checkmate::assert_character()`.
  This printed "Loading required namespace: testthat" on the first fit of a
  session, and would have failed outright where testthat is not installed.

# famish 0.2.1

* Fixed `fit_dst("lp3", ..., method = "lmom-log")` to preserve the sign
  of the fitted log-skew, allowing a negative skew on the log scale.

# famish 0.2.0

* Initial CRAN submission.
