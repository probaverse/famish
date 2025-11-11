# Fit a Log Pearson Type III (LP3) Distribution

Fit a Log Pearson Type III (LP3) Distribution

## Usage

``` r
fit_dst_lp3(x, method = c("mge", "lmom"))
```

## Arguments

- x:

  Numeric vector from which to fit the distribution.

- method:

  Character; method used to fit the distribution. In the future, this
  may be allowed to be a fitting function when the estimation method
  requires specification, like the composite quantile estimator.
