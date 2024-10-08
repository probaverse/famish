
<!-- README.md is generated from README.Rmd. Please edit that file -->
<style>
p.comment {
background-color: #FEF8C3;
padding: 10px;
border: 1px solid black;
margin-left: 25px;
border-radius: 5px;
font-style: italic;
}

</style>

# famish <img src="man/figures/logo.png" align="right" height="134" alt="" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/probaverse/famish/branch/main/graph/badge.svg)](https://app.codecov.io/gh/probaverse/famish?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/famish)](https://CRAN.R-project.org/package=famish)
[![R-CMD-check](https://github.com/probaverse/famish/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/probaverse/famish/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of famish is to refine a **fam**ily of distributions to meet
specific requirements, often derived from real data. This includes
parameter estimation methods like maximum likelihood, as well as partial
constraints such as mean matching. The name “famish” reflects the
process of narrowing down a broad family of distributions to those that
best fit your needs.

<p class="comment">
<svg height="24" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24">
<path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 9v3.75m-9.303 3.376c-.866 1.5.217 3.374 1.948 3.374h14.71c1.73 0 2.813-1.874 1.948-3.374L13.949 3.378c-.866-1.5-3.032-1.5-3.898 0zM12 15.75h.007v.008H12z"/>
</svg>
Currently, famish is a quick-and-dirty package slapped together
as-needed to tackle project work. Expect major changes.
</p>

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
