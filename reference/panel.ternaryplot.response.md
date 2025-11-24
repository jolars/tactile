# Response Panels for Ternary Plots

Response Panels for Ternary Plots

## Usage

``` r
panel.ternaryplot.response(
  x,
  y,
  z,
  subscripts,
  response,
  region = TRUE,
  contour = TRUE,
  labels = isTRUE(contour),
  fun = c("glm", "lm"),
  formula = response ~ poly(x, y),
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

- region:

  Fill density or response estimates with a color gradient.

- contour:

  Draw contour lines for density and response estimates.

- labels:

  Label contour lines.

- fun:

  Function to apply to the response variable.

- formula:

  Formula for the function.

- ...:

  Arguments passed on to
  [`lattice::panel.lines()`](https://rdrr.io/pkg/lattice/man/llines.html),
  [`lattice::panel.polygon()`](https://rdrr.io/pkg/lattice/man/llines.html),
  [`lattice::panel.text()`](https://rdrr.io/pkg/lattice/man/llines.html).

## Value

Plots a layer inside a panel of a `lattice` plot.
