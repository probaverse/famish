# Fit Distributions by Family

Convenience wrappers around
[`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md) for each
distribution family supported by the package.

## Usage

``` r
fit_dst_bern(x, method = c("mle", "lmom"), ...)

fit_dst_beta(x, method = c("mle", "mge", "mme"), ...)

fit_dst_cauchy(x, method = c("mle", "mge"), ...)

fit_dst_chisq(x, method = c("mle", "mge", "lmom"), ...)

fit_dst_degenerate(x, method = "mle", ...)

fit_dst_empirical(x, ...)

fit_dst_exp(x, method = c("mle", "mge", "mme", "lmom"), ...)

fit_dst_f(x, method = c("mle", "mge"), ...)

fit_dst_finite(x, ...)

fit_dst_gamma(x, method = c("mle", "mge", "mme", "lmom"), ...)

fit_dst_geom(x, method = c("mle", "mme", "lmom"), ...)

fit_dst_gev(x, method = c("mle", "lmom"), ...)

fit_dst_gp(x, method = c("mle", "lmom"), ...)

fit_dst_gumbel(x, method = c("mle", "lmom"), ...)

fit_dst_lnorm(x, method = c("mle", "mge", "mme", "lmom", "lmom-log"), ...)

fit_dst_lp3(x, method = "lmom-log", ...)

fit_dst_nbinom(x, method = c("mle", "mme"), ...)

fit_dst_norm(x, method = c("mle", "mge", "mme", "lmom"), ...)

fit_dst_null(x, ...)

fit_dst_pearson3(x, method = "lmom", ...)

fit_dst_pois(x, method = c("mle", "mme", "lmom"), ...)

fit_dst_t(x, method = c("mle", "mge"), ...)

fit_dst_unif(x, method = c("mle", "mge", "mme", "lmom"), ...)

fit_dst_weibull(x, method = c("mle", "mge", "lmom"), ...)
```

## Arguments

- x:

  Numeric vector of observations to fit.

- method:

  Estimation method to use. Available options depend on the distribution
  family and are enforced via
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).

- ...:

  Additional arguments passed on to
  [`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md).

## Value

A distribution object made by the 'distionary' package.

## Details

Each helper simply forwards to
[`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md) with the
associated `family` value indicated by the function suffix.

Some families do not have a unique fitting method where it is not
applicable. These are the 'finite' ones (including 'degenerate' and
'empirical'), and the 'Null' distribution.

### Missing Combinations

Some combinations of families and methods are not supported, when one
might think that they should be. These combinations are:

- 'gp' fit by 'mge'

- 'lp3' fit by 'mle' and 'mge'

- 'pearson3' fit by 'mle' and 'mge'

- 'gev' fit by 'mge'

They are not included because of how the
[`fitdistrplus::fitdist()`](https://lbbe-software.github.io/fitdistrplus/reference/fitdist.html)
function looks for those distributional representations (i.e., `plp3()`,
`dlp3()`, etc.): since we cannot guarantee that it will find the correct
ones, these combinations are omitted for safety.

To elaborate, the current version of the wrapped
[`fitdistrplus::fitdist()`](https://lbbe-software.github.io/fitdistrplus/reference/fitdist.html)
function looks for these representations starting from the
`namespace:fitdistrplus` environment. `famish` cannot control what's
found in that search path until the global environment, but that's too
late because that's where behaviour becomes conditional on the user's
actions. This is not a problem for other distributions because these are
found in the `namespace:base` environment before the global environment
is reached.

## See also

[`fit_dst()`](https://famish.netlify.app/reference/fit_dst.md)

## Examples

``` r
# Calls can be quite simple.
fit_dst_norm(1:10)
#> Normal distribution (continuous) 
#> --Parameters--
#>     mean       sd 
#> 5.500000 2.872281 
fit_dst_gumbel(2:6)
#> Generalised Extreme Value distribution (continuous) 
#> --Parameters--
#> location    scale    shape 
#> 3.296709 1.266557 0.000000 

# Still have access to the functionality available through `fit_dst()`
x <- c(1, 4, 3, NA, 5)
fit_dst_lnorm(x, method = "lmom", na_action = "null")
#> Null distribution (NA) 
fit_dst_lnorm(x, method = "lmom", na_action = "drop")
#> Log Normal distribution (continuous) 
#> --Parameters--
#>   meanlog     sdlog 
#> 0.9931290 0.6091404 

# Fitting by l-moments on the log-scale not the same as original scale.
fit_dst_lnorm(x, method = "lmom-log", na_action = "drop")
#> Log Normal distribution (continuous) 
#> --Parameters--
#>   meanlog     sdlog 
#> 1.0235861 0.7556555 
```
