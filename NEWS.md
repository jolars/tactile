# tactile 0.0.0.9000

## New functions
* `xyplot.lm()` is an `xyplot` method that mimics the behavior of
`stats::plot.lm`.
* `ACF()`, `CCF()`, and `PACF()` are lattice-based verions of the
autocorrelation and autocovariance functions from the the `stats` package.
* `xyplot.acf()` is a plot method for `ACF()`, `CCF()`, and `PACF()`.
* `arma_acf()` is a wrapper around `stats::ARMAacf()` which adds the posibility
of plotting the result via `lattice::xyplot`.

