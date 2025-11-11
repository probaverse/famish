# Select a Distribution from Quantiles

Select the distribution that most closely follows the specified
quantiles, in terms of maximum likelihood. This is useful if you have a
list of quantiles from a distribution, but don't know the parameters;
or, if you have elicited quantiles by expert judgement, and want the
closest distribution.

## Usage

``` r
dst_from_quantiles(quantiles, probs, data, family, n = 1000, start)
```

## Arguments

- quantiles:

  Vector of quantiles.

- probs:

  Vector of non-exceedance probabilities corresponding to the quantiles.

- data:

  Optional; data frame containing the quantiles and probabilities. If
  supplied, \`quantiles\` and \`probs\` can remain unquoted.

- family:

  Name of the distribution to fit.

- n:

  Number of data points to use in the fitting.

- start:

  Optional; named list of starting parameters used in the fitting
  procedure.

## Value

A probability distribution.

## Details

Parameters are selected resulting in the largest likelihood by creating
a censored dataset. Interval-censored data are created between two
consecutive quantiles, and left- and right-censored data are created for
the smallest and largest quantiles. A total of \`n\` censored
observations are created.

If the \`start\`ing parameters are not specified, they are found by
fitting a distribution as if the quantiles are data.

If \`NA\`s are found, they are removed, along with the corresponding
\`probs\` or \`quantile\` entry.

## Examples

``` r
library(distionary)
d1 <- distionary::dst_gev(0, 1, 1)
qf <- distionary::enframe_quantile(d1, at = c(0.1, 0.5, 0.65, 0.7, 0.8, 0.9),
                                   arg_name = "prob")

## Retrieve the GEV parameters
matching <- dst_from_quantiles(quantile, prob, data = qf, family = "gev")
distionary::parameters(matching)
#> $location
#> [1] -0.003928957
#> 
#> $scale
#> [1] 0.9936664
#> 
#> $shape
#> [1] 0.9984534
#> 

## Find the closest Log Pearson Type III distribution having these quantiles
closest_lp3 <- dst_from_quantiles(
  quantile, prob, data = qf, family = "pearson3"
)
distionary::parameters(closest_lp3)
#> $mean
#> [1] 2.308462
#> 
#> $sd
#> [1] 4.743186
#> 
#> $skew
#> [1] 3.286967
#> 

## Elicit quantiles from expert judgement, and fit a Pearson Type III
judgement <- data.frame(prob = c(0.25, 0.5, 0.75),
                        quantile = c(40, 100, 200))
fit <- dst_from_quantiles(quantile, prob, data = judgement,
                          family = "pearson3")
distionary::parameters(fit)
#> $mean
#> [1] 142.7415
#> 
#> $sd
#> [1] 143.3811
#> 
#> $skew
#> [1] 1.929726
#> 
```
