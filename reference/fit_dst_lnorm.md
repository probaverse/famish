# Fit a Log-Normal Distribution

Fit a Log-Normal Distribution

## Usage

``` r
fit_dst_lnorm(x, method = c("mle", "lmom", "mom", "mge"))
```

## Arguments

- x:

  Numeric vector from which to fit the distribution.

- method:

  Character; method used to fit the distribution. In the future, this
  may be allowed to be a fitting function when the estimation method
  requires specification, like the composite quantile estimator.

## Value

A distplyr distribution.
