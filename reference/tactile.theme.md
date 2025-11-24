# Tactile Theme

A custom theme for lattice that tries to make away with some of the (in
this author's opinion) excessive margins that result from the default
settings. It also provides a different color theme based partly on
[`latticeExtra::custom.theme()`](https://rdrr.io/pkg/latticeExtra/man/custom.theme.html).

## Usage

``` r
tactile.theme(fontsize = c(12, 8), color = TRUE, ...)
```

## Arguments

- fontsize:

  A vector of two numeric scalars for text and symbols respectively.

- color:

  Colorized theme.

- ...:

  Additional named options.

## Value

A list of graphical parameters that for instance could be supplied
inside a call to
[`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html) or
set via
[`lattice::lattice.options()`](https://rdrr.io/pkg/lattice/man/lattice.options.html).

## Details

The theme currently modifies the default lattice theme so that

- paddings (margins) are minimized,

- axis tick lengths are halved, and

- title size is decreased *slightly*.

## Examples

``` r
xyplot(speed ~ dist, data = cars, par.settings = tactile.theme())

opars <- trellis.par.get()
trellis.par.set(tactile.theme())
show.settings()

trellis.par.set(opars)
```
