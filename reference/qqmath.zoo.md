# Q-Q Plots for Zoo Objects

Draw quantile-Quantile plots of a sample against a theoretical
distribution, possibly conditioned on other variables.

## Usage

``` r
# S3 method for class 'zoo'
qqmath(
  x,
  data = NULL,
  xlab = "Theoretical quantiles",
  ylab = "Sample quantiles",
  ref = TRUE,
  ci = TRUE,
  ...
)
```

## Arguments

- x:

  A `zoo` object

- data:

  Ignored

- xlab:

  X axis label

- ylab:

  Y axis label

- ref:

  Plot a reference line via
  [`lattice::panel.qqmathline()`](https://rdrr.io/pkg/lattice/man/panel.qqmathline.html).

- ci:

  Plot confidence levels via
  [`panel.qqmathci()`](https://jolars.github.io/tactile/reference/panel.qqmathci.md).

- ...:

  Parameters to pass on to
  [`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html).

## Value

Plots and returns a `trellis` object.

## See also

[`lattice::qqmath()`](https://rdrr.io/pkg/lattice/man/qqmath.html),
[`zoo::zoo()`](https://rdrr.io/pkg/zoo/man/zoo.html),
[`lattice::panel.qqmathline()`](https://rdrr.io/pkg/lattice/man/panel.qqmathline.html).

## Author

Original by Deepayan Sarkar.

## Examples

``` r
if (require(zoo))
  qqmath(zoo(lh))
#> Loading required package: zoo
#> 
#> Attaching package: ‘zoo’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.Date, as.Date.numeric
```
