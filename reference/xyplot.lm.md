# Lattice plot diagnostics for lm objects

Lattice plot diagnostics for `lm` objects, mostly mimicking the behavior
of [`stats::plot.lm()`](https://rdrr.io/r/stats/plot.lm.html) but based
on [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)
instead.

## Usage

``` r
# S3 method for class 'lm'
xyplot(
  x,
  data = NULL,
  which = c(1:3, 5),
  main = FALSE,
  id.n = 3,
  labels.id = names(residuals(x)),
  cex.id = 0.75,
  cook.levels = c(0.5, 1),
  label.pos = c(4, 2),
  layout = NULL,
  ...
)
```

## Arguments

- x:

  `lm` object, typically result of
  [`lm`](https://rdrr.io/r/stats/lm.html) or
  [`glm`](https://rdrr.io/r/stats/glm.html).

- data:

  Only provided for method consistency and is ignored.

- which:

  if a subset of the plots is required, specify a subset of the numbers
  `1:6`

- main:

  if `TRUE` plots default titles. Can also be a `list` or character
  vector of length 6.

- id.n:

  number of points to be labelled in each plot, starting with the most
  extreme.

- labels.id:

  vector of labels, from which the labels for extreme points will be
  chosen. `NULL` uses observation numbers.

- cex.id:

  magnification of point labels.

- cook.levels:

  levels of Cook's distance at which to draw contours.

- label.pos:

  positioning of labels, for the left half and right half of the graph
  respectively, for plots 1-3, 5, 6.

- layout:

  a numeric vector with `[columns, rows]` to use in the call to
  [`gridExtra::grid.arrange()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html),
  or a layout matrix which will then be passed as the `layout_matrix` in
  `grid.arrange()`.

- ...:

  arguments to be passed to
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).

## Value

A list of `trellis` objects or a single `trellis` object.

## See also

[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::plot.lm()`](https://rdrr.io/r/stats/plot.lm.html),
[`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

## Author

Original by John Maindonald and Martin Maechler. Adaptation to lattice
by Johan Larsson.

## Examples

``` r
fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
xyplot(fit)

xyplot(fit, which = 5)
```
