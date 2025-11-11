# Pearson Type III distribution quantities

p/d/q/r functions for the Pearson Type III distribution.

## Usage

``` r
dpearson3(x, mean, sd, skew)

qpearson3(p, mean, sd, skew)

ppearson3(q, mean, sd, skew)

rpearson3(n, mean, sd, skew)
```

## Arguments

- x, q:

  Vector of quantiles/magnitudes.

- mean, sd, skew:

  Parameters

- p:

  Vector of probabilities.

- n:

  Number of observations to draw.

## Author

For now, the function bodies are copied from the USGS smwrBase package
because it's not available for newer R versions. This will be changed in
the future, definitely before CRAN submission. (These functions will
also be moved to somewhere else, like distionary).
