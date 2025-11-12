# famish: Flexibly Tune Probability Distributions

Delivers the estimation layer for the 'probaverse' suite of R packages
by connecting fitting routines with 'probaverse' distribution objects.
Supports multiple estimation methods, such as maximum likelihood and
L-moments, and produces distribution objects ready for downstream
manipulation and diagnostics-based evaluation.

## Overview

The famish package tunes probability distribution families so that they
align with empirical data or expert-specified targets while remaining
compatible with the [probaverse ecosystem](https://probaverse.com). With
famish, you can:

1.  Fit distribution families with a single interface that wraps
    established estimation routines.

2.  Obtain results as `distionary` objects for downstream manipulation
    and comparison.

3.  Access empirical ranking and quantile score as diagnostics.

The package acts as the estimation layer of probaverse, providing a
bridge from distribution specifications to calibrated models.

## Fitting Distribution Families

Use [`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md) to
fit any supported family by specifying the distribution family name and
estimation method. Convenience wrappers such as
[`fit_dst_gev()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md),
[`fit_dst_gp()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md),
and other `fit_dst_*()` functions expose documented combinations of
families and fitting approaches. Supported methods include maximum
likelihood, maximum goodness-of-fit method, method of moments, and
L-moments (including log-scale variants for selected families). For
references to these different methods, see the `fitdistrplus` package,
or the `lmom` package for estimation by L-moments.

## Getting Started

New users can begin with:

- the README for an overview of package goals and examples,

- [`vignette("fitting", package = "famish")`](https://famish.netlify.app/articles/fitting.md)
  for a tutorial on fitting workflows.

## See also

Useful links:

- <https://famish.probaverse.com/>

## Author

**Maintainer**: Vincenzo Coia <vincenzo.coia@gmail.com> \[copyright
holder\]

## Examples

``` r
library(distionary)

set.seed(2024)
sample_data <- rgamma(100, shape = 2, rate = 0.4)

fitted_gamma <- fit_dst("gamma", x = sample_data, method = "mle")
#> Loading required namespace: testthat
fitted_gamma
#> Gamma distribution (continuous) 
#> --Parameters--
#>     shape      rate 
#> 1.9533604 0.3551773 

parameters(fitted_gamma)
#> $shape
#> [1] 1.95336
#> 
#> $rate
#> [1] 0.3551773
#> 
enframe_return(fitted_gamma, at = c(2, 5, 10))
#> # A tibble: 3 × 2
#>    .arg return
#>   <dbl>  <dbl>
#> 1     2   4.59
#> 2     5   8.26
#> 3    10  10.8 
```
