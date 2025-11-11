# Fit Distributions by Family

Convenience wrappers around \`fit_dst()\` for each distribution family
supported by the package.

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

fit_dst_gev(x, method = c("mle", "mge", "lmom"), ...)

fit_dst_gp(x, method = c("mle", "mge", "lmom"), ...)

fit_dst_gumbel(x, method = c("mle", "lmom"), ...)

fit_dst_lnorm(x, method = c("mle", "mge", "mme", "lmom", "lmom-log"), ...)

fit_dst_lp3(x, method = c("mle", "mge", "lmom-log"), ...)

fit_dst_nbinom(x, method = c("mle", "mme"), ...)

fit_dst_norm(x, method = c("mle", "mge", "mme", "lmom"), ...)

fit_dst_null(x, ...)

fit_dst_pearson3(x, method = c("mle", "mge", "lmom"), ...)

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
  family and are enforced via \`rlang::arg_match()\`.

- ...:

  Additional arguments passed on to \`fit_dst()\`.

## Value

A distribution object made by the 'distionary' package.

## Details

Each helper simply forwards to \`fit_dst()\` with the associated
\`family\` value indicated by the function suffix.

Some families do not have a unique fitting method where it is not
applicable. These are the 'finite' ones (including 'degenerate' and
'empirical'), and the 'Null' distribution.

## See also

\[fit_dst()\]

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
