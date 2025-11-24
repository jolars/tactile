# Q-Q Diagram Confidence Intervals Panels

Panel function to go along with
[`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html) and
[`lattice::panel.qqmathline()`](https://rdrr.io/pkg/lattice/man/panel.qqmathline.html).
Adds filled confidence bands to the Q-Q-plot.

## Usage

``` r
panel.qqmathci(
  x,
  y = x,
  distribution = qnorm,
  probs = c(0.25, 0.75),
  qtype = 7,
  groups = NULL,
  ci = 0.95,
  alpha = 0.25,
  col = trellis.par.get("plot.line")$col,
  ...,
  col.line
)
```

## Arguments

- x:

  The original sample, possibly reduced to a fewer number of quantiles,
  as determined by the `f.value` argument to `qqmath`

- y:

  an alias for `x` for backwards compatibility

- distribution:

  quantile function for reference theoretical distribution.

- probs:

  numeric vector of length two, representing probabilities.
  Corresponding quantile pairs define the line drawn.

- qtype:

  the `type` of quantile computation used in
  [`quantile`](https://rdrr.io/r/stats/quantile.html)

- groups:

  optional grouping variable. If non-null, a line will be drawn for each
  group.

- ci:

  Confidence level

- alpha:

  Alpha level for the color fill

- col:

  Color fill for the confidence bands.

- ...:

  Arguments passed to
  [`lattice::panel.superpose()`](https://rdrr.io/pkg/lattice/man/panel.superpose.html)
  and
  [`lattice::panel.polygon()`](https://rdrr.io/pkg/lattice/man/llines.html)

- col.line:

  Color fill for the confidence bands. Is used internally by
  [`lattice::panel.superpose()`](https://rdrr.io/pkg/lattice/man/panel.superpose.html)
  and should generally not be changed.

## Value

Augments a trellis plot panel, such as that created by
[`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html), with
confidence levels.

## Details

The function tries to figure out the density function counterpart to
that provided in the argument `distribution` by regular expressions.

## See also

[`lattice::panel.qqmathline()`](https://rdrr.io/pkg/lattice/man/panel.qqmathline.html),
[`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html), and
[`lattice::panel.qqmath()`](https://rdrr.io/pkg/lattice/man/panel.qqmath.html).

## Author

Johan Larsson.

## Examples

``` r
qqmath(~ height | voice.part, aspect = "xy", data = singer,
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathci(x, ...)
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
```
