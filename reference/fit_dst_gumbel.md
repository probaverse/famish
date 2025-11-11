# Fit Gumbel distribution

Fits a Gumbel distribution via maximum likelihood, with
\`ismev::gum.fit()\`, and returns a distplyr distribution.

## Usage

``` r
fit_dst_gumbel(x, method = c("mle", "lmom", "mom", "mge"), diagnostics = FALSE)
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

## Value

A distplyr distribution.
