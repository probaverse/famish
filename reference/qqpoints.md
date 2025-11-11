# QQ plot pairs

QQ plot pairs

## Usage

``` r
qqpoints(x, y, xname = "x_quantile", yname = "y_quantile", n = 1000)
```

## Arguments

- x, y:

  distionary distribution, or numeric vector.

- xname, yname:

  Names of the x and y columns.

- n:

  minimum number of points to include in the plot.

## Value

A data frame of pairs, named by \`xname\` and \`yname\`.

## Details

The quantile functions of \`x\` and \`y\` are evaluated on a grid of
\`n\` quantile levels.
