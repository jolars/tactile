# Key Setup

Try to setup a key while also dodging existing keys

## Usage

``` r
setup_key(
  legend,
  key,
  default_key,
  fun,
  pos = c("right", "top", "bottom", "left")
)
```

## Arguments

- legend:

  a list of legends, usually the `$legend` slot of a trellis object.

- key:

  A key specification, usually the user input.

- default_key:

  The default key specifications that may be overridden by the user.

- fun:

  The function to draw the key, such as
  [`lattice::draw.colorkey()`](https://rdrr.io/pkg/lattice/man/draw.colorkey.html).

- pos:

  Preferences for the position of the new key.

## Value

The original `legend` object with the addition of the key defined by
`key`, `default_key`, and `fun`.
