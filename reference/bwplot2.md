# An extended box and whiskers plot

An extended version of
[`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html). The
only modification is to group and stack box plots if `groups` is
provided.

## Usage

``` r
bwplot2(x, data = NULL, ...)

# S3 method for class 'formula'
bwplot2(
  x,
  data = NULL,
  allow.multiple = is.null(groups) || outer,
  outer = FALSE,
  auto.key = FALSE,
  groups = NULL,
  drop.unused.levels = lattice.getOption("drop.unused.levels"),
  ...,
  subset = TRUE
)

# S3 method for class 'numeric'
bwplot2(x, data = NULL, xlab = deparse(substitute(x)), ...)
```

## Arguments

- x:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- data:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- ...:

  arguments passed down to
  [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- allow.multiple:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- outer:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- auto.key:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- groups:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- drop.unused.levels:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- subset:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

- xlab:

  see [`lattice::bwplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)

## Value

An object of class `"trellis"`. The
[`update`](https://rdrr.io/pkg/lattice/man/update.trellis.html) method
can be used to update components of the object and the
[`print`](https://rdrr.io/pkg/lattice/man/print.trellis.html) method
(usually called by default) will plot it on an appropriate plotting
device.

## Examples

``` r
bwplot2(variety ~ yield,
        groups = site,
        data = barley,
        par.settings = tactile.theme())
```
