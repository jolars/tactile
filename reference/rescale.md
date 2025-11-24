# Uniform Rescaling

Uniform Rescaling

## Usage

``` r
rescale(
  x,
  new_min = 0,
  new_max = 1,
  old_min = min(x, na.rm = TRUE),
  old_max = max(x, na.rm = TRUE)
)
```

## Arguments

- x:

  A numeric vector to rescale

- new_min:

  New min

- new_max:

  New max

- old_min:

  Old min

- old_max:

  Old max

## Value

A rescaled version of `x`.
