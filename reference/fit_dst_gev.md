# Fit GEV distribution

Fit GEV distribution

## Usage

``` r
fit_dst_gev(
  x,
  method = c("mle", "lmom", "mom", "mge"),
  diagnostics = FALSE,
  ...
)
```

## Arguments

- x:

  Numeric vector from which to fit the distribution.

- method:

  Character; method used to fit the distribution. In the future, this
  may be allowed to be a fitting function when the estimation method
  requires specification, like the composite quantile estimator.

- diagnostics:

  Logical; print out diagnostic plots of the fit?

- ...:

  Unused; included here for extensibility.

## Value

A distplyr distribution.
