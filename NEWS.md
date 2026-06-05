# famish (development version)

* Added composite quantile estimation (`cqe()`) and composite expectile
  estimation (`cee()`). These fit a distribution family by matching its
  quantile or expectile function to the data over a range of levels, with a
  `tau_cutoff` to concentrate the fit on the upper tail and an optional
  `weight` function. Pass the result to `fit_dst()` as `method`, or use the
  string shortcuts `method = "cqe"` / `method = "cee"`. Composite expectile
  estimation requires a 'distionary' version that provides `eval_expectile()`.

# famish 0.2.1

* Fixed `fit_dst("lp3", ..., method = "lmom-log")` to preserve the sign
  of the fitted log-skew, allowing a negative skew on the log scale.

# famish 0.2.0

* Initial CRAN submission.
