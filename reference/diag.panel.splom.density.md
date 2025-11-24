# Diagonal Density Panels

Plots univariate density estimates estimates to be used in a
[`lattice::splom()`](https://rdrr.io/pkg/lattice/man/splom.html) call
with the `diag.panel` argument.

## Usage

``` r
diag.panel.splom.density(
  x,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  weights = NULL,
  n = 512,
  ...
)
```

## Arguments

- x:

  data vector corresponding to that row / column (which will be the same
  for diagonal 'panels').

- bw:

  the smoothing bandwidth to be used. The kernels are scaled such that
  this is the standard deviation of the smoothing kernel. (Note this
  differs from the reference books cited below.)

  `bw` can also be a character string giving a rule to choose the
  bandwidth. See [`bw.nrd`](https://rdrr.io/r/stats/bandwidth.html).  
  The default, `"nrd0"`, has remained the default for historical and
  compatibility reasons, rather than as a general recommendation, where
  e.g., `"SJ"` would rather fit, see also Venables and Ripley (2002).

  The specified (or computed) value of `bw` is multiplied by `adjust`.

- adjust:

  the bandwidth used is actually `adjust*bw`. This makes it easy to
  specify values like ‘half the default’ bandwidth.

- kernel:

  the smoothing kernel to be used. See
  [`stats::density()`](https://rdrr.io/r/stats/density.html) for
  options.

- weights:

  numeric vector of non-negative observation weights, hence of same
  length as `x`. The default `NULL` is equivalent to
  `weights = rep(1/nx, nx)` where `nx` is the length of (the finite
  entries of) `x[]`. If `na.rm = TRUE` and there are `NA`'s in `x`, they
  *and* the corresponding weights are removed before computations. In
  that case, when the original weights have summed to one, they are
  re-scaled to keep doing so.

  Note that weights are *not* taken into account for automatic bandwidth
  rules, i.e., when `bw` is a string. When the weights are proportional
  to true counts `cn`, `density(x = rep(x, cn))` may be used instead of
  `weights`.

- n:

  the number of equally spaced points at which the density is to be
  estimated. When `n > 512`, it is rounded up to a power of 2 during the
  calculations (as [`fft`](https://rdrr.io/r/stats/fft.html) is used)
  and the final result is interpolated by
  [`approx`](https://rdrr.io/r/stats/approxfun.html). So it almost
  always makes sense to specify `n` as a power of two.

- ...:

  Further arguments passed on to
  [`lattice::diag.panel.splom()`](https://rdrr.io/pkg/lattice/man/panel.pairs.html)
  and
  [`lattice::panel.lines()`](https://rdrr.io/pkg/lattice/man/llines.html).

## See also

[`lattice::diag.panel.splom()`](https://rdrr.io/pkg/lattice/man/panel.pairs.html),
[`lattice::splom()`](https://rdrr.io/pkg/lattice/man/splom.html),
[`stats::density()`](https://rdrr.io/r/stats/density.html).

## Examples

``` r
splom(~ iris[1:4],
  data = iris,
  diag.panel = diag.panel.splom.density,
  pscales = 0
)
```
