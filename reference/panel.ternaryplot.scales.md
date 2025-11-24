# Axes and Labels for Ternary Plots

Axes and Labels for Ternary Plots

## Usage

``` r
panel.ternaryplot.scales(
  xlab,
  ylab,
  zlab,
  xlab.default,
  ylab.default,
  zlab.default,
  at = seq.int(0, 1, by = 0.2),
  ...
)
```

## Arguments

- xlab, ylab, zlab:

  Labels, have to be lists. Typically the user will not manipulate
  these, but instead control this via arguments to `cloud` directly.

- xlab.default:

  for internal use

- ylab.default:

  for internal use

- zlab.default:

  for internal use

- at:

  Where to draw tick marks.

- ...:

  Currently ignored.

## Value

Plots a layer inside a panel of a `lattice` plot.
