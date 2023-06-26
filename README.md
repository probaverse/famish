
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distionaryfit

<!-- badges: start -->
<!-- badges: end -->

Right now, distionaryfit is a quick-and-dirty package that simply
bundles together my quick-and-dirty functions for fitting distributions
that can be found in the vincenzocoia/distionary package.

## Installation

You can install the development version of distionaryfit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("probaverse/distionaryfit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(distionaryfit)
x <- stats::rnorm(30)
fit_dst_norm(x)
#> norm parametric dst
#> 
#>  name :
#> [1] "norm"
```
