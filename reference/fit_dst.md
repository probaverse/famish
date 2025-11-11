# Fit a distribution

Fits a probability distribution by calling a specific \`fit_dst\_\*()\`
function. Still a quick-and-dirty implementation.

## Usage

``` r
fit_dst(x, name, method, ...)
```

## Arguments

- x:

  Numeric vector to fit the distribution to.

- name:

  Name of the distribution, like \`"norm"\` or \`"gev"\`.

- method:

  Estimation method to use, like \`"mle"\` or \`"lmom"\`. For now,
  you'll have to look at the documentation of the specific fitting
  functions to see examples.

- ...:

  Other arguments to pass to the specific \`fit_dst\_\*()\` function.

## Value

A probability distribution.

## Examples

``` r
fit_dst(1:10, "norm", "mle")
#> [1] "norm"       "parametric" "dst"       
#> 
#>  name :
#> [1] "norm"
```
