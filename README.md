
<!-- README.md is generated from README.Rmd. Please edit that file -->

# famish <img src="man/figures/logo.png" align="right" height="134" alt="" />

<!-- badges: start -->
<!-- badges: end -->

Right now, famish is a quick-and-dirty package that simply bundles
together my quick-and-dirty functions for fitting some distributions
that can be found in the distionary package.

The goal of famish is to refines a **fam**ily of distributions to match
real-world data or specific characteristics, such as a given mean or
dataset. From parameter estimation to maximum likelihood, famish offers
tools for selecting the best-fitting distribution for your needs.

## Installation

You can install the development version of famish from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("probaverse/famish")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(famish)
x <- stats::rnorm(30)
fit_dst_norm(x)
#> norm parametric dst
#> 
#>  name :
#> [1] "norm"
```
