# famish (development version)

* Fixed `fit_dst("lp3", ..., method = "lmom-log")` to pass the fitted
  `lmom::pelpe3()` parameters through directly as
  `distionary::dst_lp3(meanlog = mu, sdlog = sigma, skew = gamma)`,
  preserving the sign of the fitted log-skew.

# famish 0.2.0

* Initial CRAN submission.
