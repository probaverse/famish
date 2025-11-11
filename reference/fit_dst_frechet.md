# Fit a Frechet (EV2) Distribution

Fit a Frechet (EV2) Distribution

## Usage

``` r
fit_dst_frechet(xdat, method = c("mle", "lmom", "mom", "mge"), maxit = 10000)
```

## Arguments

- xdat:

  Vector of data to fit the distribution to.

- method:

  Character; method used to fit the distribution. In the future, this
  may be allowed to be a fitting function when the estimation method
  requires specification, like the composite quantile estimator.

- maxit:

  For the \`optim()\` function.

## Note

I believe I took this code from the ismev package. If I end up
publishing this package, I'll figure something out in terms of
referencing/ permissions. Until then...
