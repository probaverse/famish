# Quantile score (pinball loss)

Computes the asymmetric absolute loss commonly used to assess quantile
forecasts. Lower scores indicate a better match between the estimated
quantile and the observed value at level `tau`.

## Usage

``` r
quantile_score(x, xhat, tau)
```

## Arguments

- x:

  Numeric vector of observed values.

- xhat:

  Numeric vector of estimated quantiles.

- tau:

  Numeric vector of quantile levels in `(0, 1)`.

## Value

Numeric vector of quantile scores corresponding to each element of the
recycled inputs.

## Details

The score minimises to zero when the observation equals the estimated
quantile, so that smaller scores indicate a better fitting model.
Positive residuals are penalised by a factor of `tau`, and negative
residuals by `tau - 1`. This loss is also known as the stick function,
check loss, asymmetric absolute deviation, or pinball loss.

For observation `x`, estimate `x_hat`, and level `tau`, the score (c.f.
Gneiting, 2011) is

\$\$ S\_\tau(x, \hat{x}) = \begin{cases} \tau \|x - \hat{x})\|, & x \ge
\hat{x}, \\ (1 - \tau)\|x - \hat{x}\|, & x \< \hat{x}. \end{cases} \$\$

Vector recycling of all three arguments follows the rules in
[`vctrs::vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.html).

## References

Gneiting, T. (2011). Making and evaluating point forecasts. *Journal of
the American Statistical Association*, 106(494), 746–762.
