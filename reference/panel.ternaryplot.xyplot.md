# Ternary Plot Wrapper for lattice::xyplot

This mainly exists to enable users to string together their own ternary
plot functions.

## Usage

``` r
panel.ternaryplot.xyplot(x, y, z, subscripts, ...)
```

## Arguments

- x:

  Numeric vector of values in the original space

- y:

  Numeric vector of values in the original space

- z:

  Numeric vector of values in the original space

- subscripts:

  see
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).

- ...:

  Arguments that are passed on to
  [`lattice::panel.xyplot()`](https://rdrr.io/pkg/lattice/man/panel.xyplot.html).

## Value

Plots a layer inside a panel of a `lattice` plot.
