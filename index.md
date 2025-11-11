# famish

The goal of famish is to refine a **fam**ily of distributions to meet
specific requirements, often derived from real data. This includes
parameter estimation methods like maximum likelihood, as well as partial
constraints such as mean matching. The name “famish” reflects the
process of narrowing down a broad family of distributions to those that
best fit your needs.

![](data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjI0IiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAyNCAyNCI+PHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLXdpZHRoPSIxLjUiIGQ9Ik0xMiA5djMuNzVtLTkuMzAzIDMuMzc2Yy0uODY2IDEuNS4yMTcgMy4zNzQgMS45NDggMy4zNzRoMTQuNzFjMS43MyAwIDIuODEzLTEuODc0IDEuOTQ4LTMuMzc0TDEzLjk0OSAzLjM3OGMtLjg2Ni0xLjUtMy4wMzItMS41LTMuODk4IDB6TTEyIDE1Ljc1aC4wMDd2LjAwOEgxMnoiIC8+PC9zdmc+)
Currently, famish is a quick-and-dirty package slapped together
as-needed to tackle project work. Expect major changes.

## Installation

You can install the development version of famish from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("probaverse/famish")
```

## Example

Simple example:

``` r
library(famish)
x <- stats::rnorm(30)
fit_dst_norm(x)
#> norm parametric dst
#> 
#>  name :
#> [1] "norm"
```

More realistic example (under development):

Experts have judged the probability of failure (PoF) of an engineered
structure to be 5%, and are 90% certain that the PoF falls between 1%
and 10%. What distribution should be fit to the PoF?

1.  Since PoF must be between 0 and 1, we may start with the Beta family
    of distributions.

``` r
# fam <- dst_beta()
```

2.  We for sure want the expected value to be 5%, so we restrict the
    family to only those Beta distributions with a mean of 5%:

``` r
# fam2 <- restrict(fam, ...)
```

3.  Finally narrow the family down to the distribution whose 5th and
    95th percentiles are close to 1% and 10%, respectively:

``` r
# resolve(fam2, ...)
```

Together in a pipe:

``` r
# dst_beta() |> 
#   restrict(...) |>
#   resolve(...)
```
