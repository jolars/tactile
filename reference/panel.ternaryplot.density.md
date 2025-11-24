# Two-Dimensional Density Estimation for Ternary Plots

Two-Dimensional Density Estimation for Ternary Plots

## Usage

``` r
panel.ternaryplot.density(
  x,
  y,
  z,
  subscripts,
  n = 100,
  region = TRUE,
  contour = FALSE,
  labels = isTRUE(contour),
  density_breaks = NULL,
  ...
)
```

## Arguments

- x:

  Numeric vector

- y:

  Numeric vector

- z:

  Numeric vector

- subscripts:

  See
  [`lattice::panel.xyplot()`](https://rdrr.io/pkg/lattice/man/panel.xyplot.html).

- n:

  Number of grid points in each direction. Can be scalar or a length-2
  integer vector.

- region:

  Fill density or response estimates with a color gradient.

- contour:

  Draw contour lines for density and response estimates.

- labels:

  Label contour lines.

- density_breaks:

  Breaks for the density plot if used (see
  `panel.ternaryplot.density()`).

- ...:

  Arguments that will be passed on to
  [`lattice::panel.lines()`](https://rdrr.io/pkg/lattice/man/llines.html),
  [`lattice::panel.polygon()`](https://rdrr.io/pkg/lattice/man/llines.html),
  and
  [`lattice::panel.text()`](https://rdrr.io/pkg/lattice/man/llines.html).

## Value

Plots a layer inside a panel of a `lattice` plot.
