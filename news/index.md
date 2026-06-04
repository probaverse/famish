# Changelog

## famish (development version)

- Fixed `fit_dst("lp3", ..., method = "lmom-log")` to pass the fitted
  [`lmom::pelpe3()`](https://rdrr.io/pkg/lmom/man/pel-functions.html)
  parameters through directly as
  `distionary::dst_lp3(meanlog = mu, sdlog = sigma, skew = gamma)`,
  preserving the sign of the fitted log-skew.

## famish 0.2.0

CRAN release: 2025-12-08

- Initial CRAN submission.
