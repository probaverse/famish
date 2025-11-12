
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
&#10;</style>

# famish <img src="man/figures/logo.png" align="right" height="134" alt="" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/probaverse/famish/branch/main/graph/badge.svg)](https://app.codecov.io/gh/probaverse/famish?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/famish)](https://CRAN.R-project.org/package=famish)
[![R-CMD-check](https://github.com/probaverse/famish/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/probaverse/famish/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `famish` is to refine a **fam**ily of distributions to match
a provided dataset. In most use cases, this means fitting a distribution
to data, and this is the current version functionality of `famish`.
Importantly, `famish` grounds the broader [probaverse
suite](https://probaverse.com/) of packages to real data.

This young version of `famish` currently works mostly by wrapping
existing fitting functions from other packages, particularly
`fitdistrplus`, `ismev` and `lmom`. The main function is `fit_dst()`,
which fits a specified distribution family to data using a specified
fitting method. Thin wrappers for specific distribution families are
also provided, such as `fit_dst_gev()` for the Generalised Extreme Value
distribution.

The name “famish” reflects the process of narrowing down a broad family
of distributions to those that best fit your needs.

## Statement of Need

Many software routines allow for the estimation of probability
distributions, but there is a need to connect those estimates to the
downstream operations needed for advanced statistical models. The
probaverse supplies that higher-level infrastructure, and `famish` is
the bridge that grounds probaverse-built models in real datasets or
expert judgment.

## Target Audience

`famish` supports the probaverse user base – anyone who works with
probability distributions, including data scientists, analysts,
researchers, and students. It serves users who need flexible fitting
workflows and clear diagnostics for how well a distribution matches
observed data.

It is particularly useful for risk-focused domains – hydrology,
economics, actuarial science, credit risk, and similar fields – where
tail behaviour and extremes determine decisions and advanced
probabilistic models rely on dependable estimation tools.

## Installation

You can install the development version of famish from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("probaverse/famish")
```

## Future Goals

While the current version of `famish` is limited in scope, it has big
long-term goals, especially as the broader probaverse expands to allow
for the easier creation of distribution families. Some bigger goals for
`famish` include:

- Fitting cascades, such as first refining a family to have a specified
  mean (e.g., as estimated by regression), and then estimating the
  remaining parameters.
- Fitting a distribution to best match a supplied table of quantiles, or
  another reference distribution.
- Providing modern estimation methods that are more appropriate for
  estimation of risk and hazard analysis.

Additional features will be added as development continues. We
appreciate your patience and welcome contributions! Please see the
[contributing guide](.github/CONTRIBUTING.md) to get started.

## Example: Quick Streamflow Fit

For a complete walkthrough, including comparisons between multiple
fitted families and tail-focused diagnostics, see the [fitting
vignette](articles/fitting.html). The snippet below shows the minimal
workflow.

``` r
library(distionary)
library(famish)
```

Sample dataset: annual streamflow maxima (cms) for 12 years.

``` r
x <- c(4.0, 2.7, 3.5, 3.2, 7.1, 3.1, 2.5, 5.0, 2.3, 4.5, 3.0, 3.8)
```

Fit a Generalised Extreme Value distribution via maximum likelihood.

``` r
d <- fit_dst_gev(x)
#> Loading required namespace: testthat
d
#> Generalised Extreme Value distribution (continuous) 
#> --Parameters--
#>  location     scale     shape 
#> 3.0658476 0.7426435 0.2699160
```

Distributions are objects understood by the `distionary` package, so you
can use all the familiar methods to inspect and work with them. For
example, calculate its mean:

``` r
mean(d)
#> [1] 3.761526
```

The `fit_dst()` function is the main fitting function in `famish`. Here
is an example, this time fitting a Normal distribution by L-moments.

``` r
fit_dst("norm", x = x, method = "lmom")
#> Normal distribution (continuous) 
#> --Parameters--
#>     mean       sd 
#> 3.725000 1.279658
```

## Correctness and Reliability

For those combinations of distribution families and fitting methods
indicated by the functions `fit_dst_*()`, rigorous testing has been
conducted to ensure that the estimation methods are consistent – that
is, the estimated distribution parameters converge to the true parameter
values as more data are drawn from the distribution being estimated.

## `famish` in the Context of Other Packages

`famish` is unique as it is a bridge from existing fitting routines to
the probaverse suite of packages.

- Packages `lmom`, `ismev`, and `fitdistrplus` are all useful for
  fitting distribution parameters (and are in fact wrapped by `famish`),
  but remain low-level.
- Packages like `distributions3` and `distributional` turn distributions
  in objects, but lack estimation capabilities.

## Acknowledgements

The creation of `famish` would not have been possible without the
support of BGC Engineering Inc., the Politecnico di Milano, and the
European Space Agency.

## Citation

To cite package `famish` in publications use:

Coia V (2025). *famish: Flexibly Tune Probability Distributions*. R
package version 0.2.0, <https://github.com/probaverse/famish>,
<https://famish.probaverse.com/>.

## Code of Conduct

Please note that the `famish` project is released with a [Code of
Conduct](https://distplyr.probaverse.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
