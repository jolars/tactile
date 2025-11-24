# Diagnostic Plots for ARIMA Models

Diagnostic plots modelled after
[`stats::tsdiag()`](https://rdrr.io/r/stats/tsdiag.html) with some
modifications and corrections of p-values in the Box–Ljung test.

## Usage

``` r
# S3 method for class 'Arima'
xyplot(
  x,
  data = NULL,
  which = 1:4,
  lag.max = NULL,
  gof.lag = NULL,
  qq.aspect = "iso",
  na.action = na.pass,
  main = NULL,
  layout = NULL,
  ...
)
```

## Arguments

- x:

  A fitted time-series model of class `Arima`.

- data:

  Ignored

- which:

  A sequence of integers between 1 and 4, identifying the plots to be
  shown.

- lag.max:

  Number of lags to compute ACF for.

- gof.lag:

  The maximum number of lags for the Ljung–Box test.

- qq.aspect:

  Aspect of Q-Q plot (see
  [`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html)).

- na.action:

  Treatment of `NA`s.

- main:

  Optional titles for the plots. Can also be `TRUE`, in which case a
  default list of titles will be added.

- layout:

  Either a numeric vector with (columns, rows) to use in the call to
  [`gridExtra::grid.arrange()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html),
  or a layout matrix which will then be passed as the `layout_matrix` in
  `grid.arrange()`.

- ...:

  Parameters to pass to
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).

## Value

Plots a lattice plot and returns a `trellis` object.

## See also

[`stats::tsdiag()`](https://rdrr.io/r/stats/tsdiag.html),
[`stats::arima()`](https://rdrr.io/r/stats/arima.html),
[`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html),
[`gridExtra::grid.arrange()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html),
[`stats::Box.test()`](https://rdrr.io/r/stats/box.test.html).

## Examples

``` r
fit <- arima(lh, order = c(1, 1, 0))
xyplot(fit, layout = c(2, 2))

xyplot(fit, which = c(1:2, 4), layout = rbind(c(1, 1), c(2, 3)))
```
