# Rank-based Scores

Converts a numeric vector to its rank-based scores. For `uscore()`
(uniform scores), values become roughly equally spaced between 0 and 1,
keeping their order. `nscore()` calculates normal scores by spacing the
uniform scores along a standard normal distribution; `rpscore()`
calculates empirical return periods by spacing the uniform scores `u` by
`1 / (1 - u)`.

## Usage

``` r
uscore(x, pos = "Hazen", na.rm = FALSE)

nscore(x, pos = "Hazen", na.rm = FALSE)

rpscore(x, pos = "Hazen", na.rm = FALSE)
```

## Arguments

- x:

  Numeric vector.

- pos:

  Positional adjustment for uniform scores. See Details. Can be a single
  numeric, or could be named after one of the proponents behind a choice
  of the numeric: "Weibull", "Beard", "Gringorten", or "Hazen".

- na.rm:

  Logical indicating whether `NA` and `NaN` values should be removed
  from the output.

## Value

Vector of uniform scores.

## Details

Uniform scores are calculated by `(rank+a)/(n+1+2*a)`, where `rank` is
the ranked `x` values, and `a` is the positional adjustment `pos`.
Alternatively, could be named after an individual associated with a
choice of `a`:

- Weibull (1939) proposed `a = 0`.

- Beard (1943) proposed `a = -0.31`.

- Gringorten (1963) proposed `a = -0.44`.

- Hazen (1914) proposed `a = -0.5`.

## Author

Thanks to Dr. Harry Joe for providing a starting framework for the
`uscore()` function.

## Examples

``` r
x <- c(0.3, 0.56, NA, 0.1, -12)
uscore(x)
#> [1] 0.625 0.875    NA 0.375 0.125
uscore(x, pos = "Gringorten")
#> [1] 0.585034 0.755102       NA 0.414966 0.244898
nscore(x, pos = -0.4)
#> [1]  0.3029804  1.0675705         NA -0.3029804 -1.0675705
rpscore(x)
#> [1] 2.666667 8.000000       NA 1.600000 1.142857
```
