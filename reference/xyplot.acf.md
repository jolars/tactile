# Plot Autocovariance and Autocorrelation Functions

This is a version of
[`stats::plot.acf()`](https://rdrr.io/r/stats/plot.acf.html).

## Usage

``` r
# S3 method for class 'acf'
xyplot(
  x,
  data = NULL,
  ci = 0.95,
  ci_type = c("white", "ma"),
  ci_col = trellis.par.get("add.line")$col,
  ci_lty = 2,
  ...
)
```

## Arguments

- x:

  An 'acf' object.

- data:

  Ignored

- ci:

  Confidence level.

- ci_type:

  Type of confidence level.

- ci_col:

  Line color for the confidence levels.

- ci_lty:

  Line type for the confidence levels.

- ...:

  Arguments passed on to
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html).

## Value

Returns and plots a `trellis` object.

## See also

[`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html),
[`stats::plot.acf()`](https://rdrr.io/r/stats/plot.acf.html),
[`stats::acf()`](https://rdrr.io/r/stats/acf.html).

## Author

Original by Brian Ripley.

## Examples

``` r
z <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
xyplot(acf(z))

```
