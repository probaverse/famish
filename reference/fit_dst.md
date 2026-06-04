# Fit a distribution

Estimation of probability distributions available in the 'distionary'
package. Wraps the 'lmom' package when fitting by L-moments, the 'ismev'
package when fitting the GP/GEV/Gumbel by MLE, and the 'fitdistrplus'
package for other combinations.

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

  Name of the target distribution family, such as `"norm"`, `"gev"`,
  `"pois"`. See details. Character vector of length 1.

- x:

  Numeric vector containing the observations to fit.

- method:

  Estimation method to use. Valid choices include `"mle"`, `"mge"`,
  `"mme"`, `"lmom"`, and `"lmom-log"`. The default is `"mle"`, although
  beware that not all families support the `"mle"` method yet (pearson3
  and lp3).

- na_action:

  Strategy for dealing with `NA` values in `x`. `"null"` returns a Null
  distribution
  ([`distionary::dst_null()`](https://distionary.probaverse.com/reference/dst_null.html));
  `"drop"` silently removes missing observations before fitting; and
  `"fail"` aborts with an error.

- on_unres:

  Behaviour when no distribution can be resolved for the supplied
  inputs. `"null"` (default) yields a Null distribution
  ([`distionary::dst_null()`](https://distionary.probaverse.com/reference/dst_null.html))
  distribution with a warning, whereas `"fail"` propagates an error.

## Value

A distribution object of class "dst" encapsulating the fitted
distribution.

## Details

The `fit_dst()` function is currently a lightweight fitting wrapper,
with pre-specified behaviour for certain `family` / `method`
combinations. A full list of families and their compatible estimation
methods is available via specific family wrappers, such as
[`fit_dst_norm()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md),
[`fit_dst_pois()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md),
etc.

Here is how fitting is implemented.

- For `method = "lmom"` and distribution families 'gamma', 'gev', 'gp',
  'gumbel', 'lnorm', 'norm', 'pearson3', and 'weibull', the 'lmom'
  package is wrapped by first calling
  [`lmom::samlmu()`](https://rdrr.io/pkg/lmom/man/samlmu.html) on the
  data vector `x` to calculate the L-moments, then the relevant
  `lmom::pel*()` function is called to estimate parameters.

- For `method = "lmom"` and distribution families 'exp', 'pois', 'bern',
  'geom', 'chisq', and 'unif', the method of L-moments is manually
  implemented. All of these families except 'unif' have a single
  parameter for which only the mean is needed (and thus is equivalent to
  the 'mme' method). The 'unif' family has minimum and maximum parameter
  values calculated as `l1 - 3 * l2` and `l1 + 3 * l2`, where `l1` and
  `l2` are the first and second L-moments (see Hosking, 1990, Table 1).

- For `method = "lmom-log"`, only the 'lnorm' and 'lp3' families are
  supported, otherwise no distribution will be resolved. The method fits
  the distributions via the 'lmom' method on the log scale. That is,
  'norm' and 'pearson3' distributions are fit on the log of the data,
  for which the respective 'lnorm' or 'lp3' distribution is obtained.
  For 'lp3', the fitted parameters from
  [`lmom::pelpe3()`](https://rdrr.io/pkg/lmom/man/pel-functions.html)
  are passed through directly as
  `distionary::dst_lp3(meanlog = mu, sdlog = sigma, skew = gamma)`.

- For `method = "mle"` and distribution families 'gev', 'gp', or
  'gumbel', the 'ismev' package is used to fit the distribution by
  maximum likelihood estimation. This is done by invoking the functions
  [`ismev::gev.fit()`](https://rdrr.io/pkg/ismev/man/gev.fit.html),
  [`ismev::gpd.fit()`](https://rdrr.io/pkg/ismev/man/gpd.fit.html) (with
  `threshold = 0`), and
  [`ismev::gum.fit()`](https://rdrr.io/pkg/ismev/man/gum.fit.html)
  (respectively).

- For `method = "mle"` and distribution family 'bern' and 'degenerate',
  the MLE is calculated manually. For 'bern', the parameter is estimated
  as the mean of the 0-1 data; for 'degenerate', the unique data value.

- For `method = "mme"` and `"lmom"`, the 'cauchy' family fails to fit
  because Cauchy distributions don't have finite moments (Feller, 1971).

- For families 'empirical' and 'finite', the empirical distribution is
  fit to the supplied data.

- For the 'null' family, a Null distribution is returned.

- For any other combination of `family` and `method`, the
  [`fitdistrplus::fitdist()`](https://lbbe-software.github.io/fitdistrplus/reference/fitdist.html)
  function is called by inserting the data `x`, the `family` name, and
  the `method`. Some distributions require starting values for the
  parameters. For the families 't', 'f', and 'chisq', this is done by
  moment matching ('mme'). For 'gev', 'gp', and 'gumbel', the MLE is
  used as starting values (through `method = "mle"`).

To understand what the distribution families are, see the documentation
in the 'distionary' package through the `dst_*()` functions. For
example, the 'lp3' family can be found at `?distionary::dst_lp3()`. Note
that the Gumbel distribution is not available yet in 'distionary', but
is simply the 'gev' family with `shape = 0`.

To understand the estimation methods, see the 'lmom' package for the
`"lmom"` method. For the `"lmom-log"` method, it is the same as
`"lmom"`, but via the log of the data and the corresponding
log-transformed distributions. For all others, see the 'fitdistrplus'
package documentation.

### Handling of missing or unresolvable data

When `na_action = "drop"`, the function operates on the subset of `x`
without missing values (via `x <- x[!is.na(x)]`). This takes priority
over behaviour indicated in `on_unres`.

If fitting fails, a Null distribution is output if `on_unres = "null"`
(the default), or an error is thrown if `on_unres = "fail"`. Fitting can
fail due to not having enough data, not being able to isolate a single
distribution, or various other reasons that would typically otherwise
result in an error or `NA` parameters in the wrapped fitting method.

## References

Hosking, J. R. M. (1990). L-moments: Analysis and estimation of
distributions using linear combinations of order statistics. *Journal of
the Royal Statistical Society: Series B (Methodological)*, 52(1),
105–124.

Feller, W. (1971). *An Introduction to Probability Theory and Its
Applications* (Vol. 2, 2nd ed.). Wiley.

## See also

`fit_dst_*()` helpers such as
[`fit_dst_norm()`](https://famish.netlify.app/reference/fit_dst_family_wrappers.md).

## Examples

``` r
fit_dst("norm", x = 1:10, method = "mle")
#> Normal distribution (continuous) 
#> --Parameters--
#>     mean       sd 
#> 5.500000 2.872281 
fit_dst("gev", x = c(1, 4, 3, NA, 5), method = "lmom", na_action = "drop")
#> Generalised Extreme Value distribution (continuous) 
#> --Parameters--
#>   location      scale      shape 
#>  3.0145621  2.1806961 -0.7498928 
fit_dst("pois", x = c(1, 4, 3, NA, 5), na_action = "null")
#> Null distribution (NA) 

# "lnorm" with "lmom-log" shares parameters with "norm" fit by "lmom".
fit_dst("lnorm", x = 1:10, method = "lmom-log")
#> Log Normal distribution (continuous) 
#> --Parameters--
#>   meanlog     sdlog 
#> 1.5104413 0.7487066 
fit_dst("norm", x = log(1:10), method = "lmom")
#> Normal distribution (continuous) 
#> --Parameters--
#>      mean        sd 
#> 1.5104413 0.7487066 
```
