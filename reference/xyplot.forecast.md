# Plot Forecasts with Trellis Graphics

Plot forecasts from
[`forecast::forecast()`](https://pkg.robjhyndman.com/forecast/reference/reexports.html).
It is built mostly to resemble the
[`forecast::autoplot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.html)
and
[`forecast::plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.html)
functions, but in addition tries to plot the predictions on the original
scale.

## Usage

``` r
# S3 method for class 'forecast'
xyplot(
  x,
  data = NULL,
  ci = TRUE,
  ci_levels = x$level,
  ci_key = ci,
  ci_pal = hcl(0, 0, 45:100),
  ci_alpha = trellis.par.get("regions")$alpha,
  ...
)
```

## Arguments

- x:

  An object of class `forecast`.

- data:

  Data of observations left out of the model fit, usually "future"
  observations.

- ci:

  Plot confidence intervals for the predictions.

- ci_levels:

  The prediction levels to plot as a subset of those forecasted in `x`.

- ci_key:

  Set to `TRUE` to draw a key automatically or provide a list (if
  `length(ci_levels)` \> 5 should work with
  [`lattice::draw.colorkey()`](https://rdrr.io/pkg/lattice/man/draw.colorkey.html)
  and otherwise with
  [`lattice::draw.key()`](https://rdrr.io/pkg/lattice/man/draw.key.html))

- ci_pal:

  Color palette for the confidence bands.

- ci_alpha:

  Fill alpha for the confidence interval.

- ...:

  Arguments passed on to
  [`lattice::panel.xyplot()`](https://rdrr.io/pkg/lattice/man/panel.xyplot.html).

## Value

An object of class `"trellis"`. The
[`update`](https://rdrr.io/pkg/lattice/man/update.trellis.html) method
can be used to update components of the object and the
[`print`](https://rdrr.io/pkg/lattice/man/print.trellis.html) method
(usually called by default) will plot it on an appropriate plotting
device.

## Details

This function requires the **zoo** package.

## See also

[`lattice::panel.xyplot()`](https://rdrr.io/pkg/lattice/man/panel.xyplot.html),
[`forecast::forecast()`](https://pkg.robjhyndman.com/forecast/reference/reexports.html),
[`lattice::xyplot.ts()`](https://rdrr.io/pkg/lattice/man/xyplot.ts.html).

## Examples

``` r
if (require(forecast)) {
  train <- window(USAccDeaths, c(1973, 1), c(1977, 12))
  test <- window(USAccDeaths, c(1978, 1), c(1978, 12))
  fit <- arima(train, order = c(0, 1, 1),
               seasonal = list(order = c(0, 1, 1)))
  fcast1 <- forecast(fit, 12)
  xyplot(fcast1, test, grid = TRUE, auto.key = list(corner = c(0, 0.99)),
         ci_key = list(title = "PI Level"))

  # A fan plot
  fcast2 <- forecast(fit, 12, level = seq(0, 95, 10))
  xyplot(fcast2, test, ci_pal = heat.colors(100))
}
#> Loading required package: forecast
```
