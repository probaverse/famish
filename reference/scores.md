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

Vector of uniform scores corresponding to values in `x`.

## Details

Uniform scores are calculated by `(rank+a)/(n+1+2*a)`, where `rank` is
the ranked `x` values, and `a` is the positional adjustment `pos`.
Alternatively, could be named after an individual associated with a
choice of `a`:

- Weibull (1939) proposed `a = 0`.

- Beard (1943) proposed `a = -0.31`.

- Gringorten (1963) proposed `a = -0.44`.

- Hazen (1914) proposed `a = -0.5`.

## References

Beard, L. R. (1943). Statistical analysis in hydrology. *Transactions of
the American Society of Civil Engineers*, 108, 1110–1160.

Gringorten, I. I. (1963). A plotting rule for extreme probability paper.
*Journal of Geophysical Research*, 68(3), 813–814.

Hazen, A. (1914). Storage to be provided in impounding reservoirs for
municipal water supply. *Transactions of the American Society of Civil
Engineers*, 77, 1539–1640.

Weibull, W. (1939). A statistical theory of the strength of materials.
*IVB-Handl.*, 151.

## Author

Thanks to Dr. Harry Joe for providing a starting framework for the
`uscore()` function.

## Examples

``` r
x <- c(0.3, 0.56, NA, 0.1, -12)
uscore(x)
#> [1] 0.625 0.875    NA 0.375 0.125
uscore(x, pos = "Gringorten")
#> [1] 0.6213592 0.8640777        NA 0.3786408 0.1359223
nscore(x, pos = -0.4)
#> [1]  0.3029804  1.0675705         NA -0.3029804 -1.0675705
rpscore(x)
#> [1] 2.666667 8.000000       NA 1.600000 1.142857
```
