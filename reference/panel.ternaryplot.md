# Panel Function for Ternary Plots

Panel Function for Ternary Plots

## Usage

``` r
panel.ternaryplot(
  x,
  y,
  z,
  subscripts,
  response = NULL,
  density = FALSE,
  region = density || !is.null(response),
  contour = density || !is.null(response),
  labels = !is.null(response),
  points = TRUE,
  grid = TRUE,
  density_breaks = NULL,
  xlab,
  ylab,
  zlab,
  xlab.default,
  ylab.default,
  zlab.default,
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

- response:

  An optional response variable

- density:

  Compute two-dimensional density estimates via
  [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html).

- region:

  Fill density or response estimates with a color gradient.

- contour:

  Draw contour lines for density and response estimates.

- labels:

  Label contour lines.

- points:

  Draw points (via
  [`panel.ternaryplot.xyplot()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.xyplot.md)).

- grid:

  Draw a reference grid.

- density_breaks:

  Breaks for the density plot if used (see
  [`panel.ternaryplot.density()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.density.md)).

- xlab:

  X axis label (the left dimension)

- ylab:

  Y axis label (the right dimension)

- zlab:

  Z axis label (the top dimension)

- xlab.default:

  Internal argument

- ylab.default:

  Internal argument

- zlab.default:

  Internal argument

- ...:

  Arguments passed down to subsequent panel functions.

## Value

Plots a layer inside a panel of a `lattice` plot.

## See also

The building blocks of this function are available as the separate panel
functions
[`panel.ternaryplot.xyplot()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.xyplot.md),
[`panel.ternaryplot.grid()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.grid.md),
[`panel.ternaryplot.scales()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.scales.md),
[`panel.ternaryplot.clip()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.clip.md),
[`panel.ternaryplot.response()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.response.md),
and
[`panel.ternaryplot.density()`](https://jolars.github.io/tactile/reference/panel.ternaryplot.density.md)
in case the user would like to get complete control of the
customization.
