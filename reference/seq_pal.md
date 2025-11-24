# Sequential palette helper.

Divides the regions palette from lattice in half when it does not make
sense to have a diverging palette.

## Usage

``` r
seq_pal(n, bias = 1, space = "Lab", ...)
```

## Arguments

- n:

  Number of colors to generate

- ...:

  Stuff to pass on to
  [`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html)
