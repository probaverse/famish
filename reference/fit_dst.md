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

  Name of the target distribution family, such as \`"norm"\`, \`"gev"\`,
  \`"pois"\`, or any other entry recognised by `available_methods()`.
  Custom families supplied via additional \`fit_dst\_\*()\` helpers are
  also supported.

- x:

  Numeric vector containing the observations to fit. Missing values are
  handled according to \`na_action\`.

- method:

  Estimation method to use. Valid choices include \`"mle"\`, \`"mge"\`,
  \`"mme"\`, \`"lmom"\`, and \`"lmom-log"\`. The default is to match
  against this set and use the first compatible option for the specified
  \`family\`.

- na_action:

  Strategy for dealing with \`NA\` or otherwise invalid values in \`x\`.
  \`"null"\` returns \`distionary::dst_null()\`, \`"drop"\` silently
  removes missing observations before fitting, and \`"fail"\` aborts
  with an error.

- on_unres:

  Behaviour when no distribution can be resolved for the supplied
  inputs. \`"null"\` (default) yields a \`distionary::dst_null()\`
  distribution with a warning, whereas \`"fail"\` propagates an error.

## Value

A \`distionary::distribution\` object encapsulating the fitted model and
its parameters.

## Details

The fitting workflow proceeds in the following order:

1.  Input validation and missing-value handling governed by
    \`na_action\`.

2.  A compatibility check via `supported_combination()` to warn about
    unsupported \`family\` / \`method\` pairs.

3.  Specialised dispatch for known fast paths, including empirical and
    finite distributions, degenerate MLE fits, extreme-value families
    backed by \`ismev\`, and L-moment workflows (including log-L-moment
    transforms).

4.  A final fallback to `wrapper_fitdistrplus()` for general-purpose
    maximum-likelihood or moment-based estimation.

If every attempt fails, the function either returns \`dst_null()\` or
raises an error depending on \`on_unres\`.

## Supported combinations

A full list of families and their compatible estimation methods is
available via `available_methods()`. The helper respects bespoke
wrappers contained in \`fit_dst-family_wrappers.R\` and will
automatically pick them up when new families are registered.

## Missing data and unresolved fits

When \`na_action\` is \`"drop"\`, the function operates on the subset of
\`x\` without missing values. For empty inputs or combinations that
cannot be resolved, \`on_unres\` determines whether a
\`distionary::dst_null()\` object is returned or an error is thrown.
This behaviour is particularly relevant for heavy-tailed families (e.g.
\`"cauchy"\`) where certain methods are known to fail.

## See also

`available_methods()`, `supported_combination()`, \`fit_dst\_\*()\`
helpers,
[`distionary::dst_null()`](https://distionary.probaverse.com/reference/dst_null.html),
[`distionary::dst_empirical()`](https://distionary.probaverse.com/reference/dst_empirical.html)

## Examples

``` r
fit_dst("norm", x = 1:10, method = "mle")
#> Loading required namespace: testthat
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

# Force an error when an unresolved fit would otherwise return a Null dst.
try(fit_dst("cauchy", x = 1:10, method = "lmom", on_unres = "fail"))
#> Error in unresolved() : Failed to resolve a distribution.

# Inspect the methods supported for a given family.
available_methods()[["gev"]]
#> Error in available_methods(): could not find function "available_methods"
```
