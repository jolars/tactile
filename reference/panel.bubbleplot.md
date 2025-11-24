# Panel Function for Bubble Plots

Panel Function for Bubble Plots

## Usage

``` r
panel.bubbleplot(x, y, z, groups = NULL, subscripts, cex = NULL, ...)
```

## Arguments

- x, y:

  variables to be plotted in the scatterplot

- z:

  A numeric vector that areas of circles will be mapped to.

- groups:

  Grouping variable (see
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)).

- subscripts:

  A vector of indexes to specify which observation to plot. Normally
  does not need to be provided by the user.

- cex:

  Is used internally and user settings will be ignored.

- ...:

  Further arguments to pass to
  [`lattice::panel.xyplot()`](https://rdrr.io/pkg/lattice/man/panel.xyplot.html).

## Value

Plots a layer inside a panel of a `lattice` plot.
