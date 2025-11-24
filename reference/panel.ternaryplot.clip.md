# Plot Region Clipping for Ternary Plots

Plot Region Clipping for Ternary Plots

## Usage

``` r
panel.ternaryplot.clip(
  xl = current.panel.limits()$x,
  yl = current.panel.limits()$y,
  border = "transparent",
  col = if (background$col == "transparent") "#FFFFFF" else background$col
)
```

## Arguments

- xl:

  X axis limits

- yl:

  Y axis limits

- border:

  Border color

- col:

  Polygon fill

## Value

Plots a layer inside a panel of a `lattice` plot.
