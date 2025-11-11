# Fit a distribution

Fits a probability distribution by calling a specific \`fit_dst\_\*()\`
function. Still a quick-and-dirty implementation.

## Usage

``` r
fit_dst(
  family,
  x,
  method = c("mle", "mge", "mme", "lmom", "lmom-log"),
  na_action = c("null", "drop", "fail"),
  on_unres = c("null", "fail")
)
```

## Arguments

- family:

  Name of the distribution, like \`"norm"\` or \`"gev"\`.

- x:

  Numeric vector to fit the distribution to.

- method:

  Estimation method to use, like \`"mle"\` or \`"lmom"\`. For now,
  you'll have to look at the documentation of the specific fitting
  functions to see examples.

- na_action:

  How to resolve missing or invalid observations in \`x\`. One of
  \`"null"\`, \`"drop"\`, or \`"fail"\`.

- on_unres:

  Behaviour when fitting does not resolve to a single distribution. One
  of \`"null"\` or \`"fail"\`.

## Value

A probability distribution.

## Examples

``` r
fit_dst("norm", x = 1:10, method = "mle")
#> Loading required namespace: testthat
#> Warning: Failed to resolve a distribution. Returning a Null distribution.
#> NULL distribution
fit_dst("gev", x = c(1, 4, 3, NA, 5), method = "lmom", na_action = "drop")
#> Warning: Failed to resolve a distribution. Returning a Null distribution.
#> NULL distribution
fit_dst("pois", x = c(1, 4, 3, NA, 5), na_action = "null")
#> NULL distribution

# If a distribution fails to fit, `on_unres` is "null" by default, returning
# a Null distribution.
fit_dst("cauchy", x = 1:10, method = "lmom")  # Cauchy moments don't exist.
#> Warning: Failed to resolve a distribution. Returning a Null distribution.
#> NULL distribution
```
