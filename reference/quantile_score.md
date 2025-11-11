# Quantile score function

Evaluates the quantile score for a given observation against a quantile
estimate.

## Usage

``` r
quantile_score(x, xhat, tau)
```

## Arguments

- x:

  Vector of observed data values.

- xhat:

  Vector of estimated quantiles.

- tau:

  Quantile level (non-exceedance probability).

## Value

The evaluated quantile score for each entry in the input vector(s).

## Note

The quantile score is based on a loss function that goes by many names –
the "asymmetric absolute deviation function", the "stick function", the
"check function", or the "pinball loss".
