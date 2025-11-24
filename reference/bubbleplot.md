# Bubbleplots

Draws bubbleblots – trivariate plots where the third dimension is mapped
to the size of the points drawn on the screen.

## Usage

``` r
bubbleplot(x, data = NULL, ...)

# S3 method for class 'formula'
bubbleplot(
  x,
  data = NULL,
  maxsize = 3,
  bubblekey = TRUE,
  panel = panel.bubbleplot,
  groups = NULL,
  subset = TRUE,
  drop.unused.levels = lattice.getOption("drop.unused.levels"),
  ...,
  outer,
  allow.multiple
)
```

## Arguments

- x:

  A formula of the form `z ~ x * y`, where `x` and `y` have the usual
  interpretation in trellis graphics (see
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html))
  and `z` is mapped to the size of bubbles.

- data:

  A data.frame, list or environment wherein the formula and groups
  arguments can be evaluated.

- ...:

  Further arguments to pass to
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).

- maxsize:

  Maximum size (in cex) for the bubbles.

- bubblekey:

  Set to `TRUE` to draw an informative legend about the bubbles. Uses
  [`lattice::draw.key()`](https://rdrr.io/pkg/lattice/man/draw.key.html).
  See the **key** section of the documentation in
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html). If
  both `auto.key` and `bubblekey` are given and their `space` arguments
  (positions) conflict, bubblekey will silently override the position of
  `auto.key`.

- panel:

  See
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).
  Here, we are passing an additional variable, `z`, which is then used
  in
  [`panel.bubbleplot()`](https://jolars.github.io/tactile/reference/panel.bubbleplot.md).

- groups:

  See [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- subset:

  See [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- drop.unused.levels:

  See [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- outer:

  Ignored.

- allow.multiple:

  Ignored.

## Value

An object of class `"trellis"`. The
[`update`](https://rdrr.io/pkg/lattice/man/update.trellis.html) method
can be used to update components of the object and the
[`print`](https://rdrr.io/pkg/lattice/man/print.trellis.html) method
(usually called by default) will plot it on an appropriate plotting
device.

## Author

Johan Larsson

## Examples

``` r
bubbleplot(disp ~ hp * wt, groups = cyl, data = mtcars, auto.key = TRUE)

bubbleplot(disp ~ hp * mpg | factor(cyl), groups = gear, data = mtcars,
           auto.key = TRUE)
```
