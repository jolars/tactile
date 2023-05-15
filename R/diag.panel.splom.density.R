#' Diagonal Density Panels
#'
#' Plots univariate density estimates estimates to be used in a
#' [lattice::splom()] call with the `diag.panel` argument.
#'
#' @param x data vector corresponding to that row / column (which will be the
#'   same for diagonal 'panels').
#' @param kernel the smoothing kernel to be used. See [stats::density()] for
#'   options.
#' @inheritParams stats::density
#' @param ... Further arguments passed on to [lattice::diag.panel.splom()] and
#'   [lattice::panel.lines()].
#' @seealso [lattice::diag.panel.splom()], [lattice::splom()],
#'   [stats::density()].
#'
#' @export
#'
#' @examples
#' splom(~ iris[1:4],
#'   data = iris,
#'   diag.panel = diag.panel.splom.density,
#'   pscales = 0
#' )
diag.panel.splom.density <- function(x,
                                     bw = "nrd0",
                                     adjust = 1,
                                     kernel = "gaussian",
                                     weights = NULL,
                                     n = 512,
                                     ...) {
  yrng <- current.panel.limits()$ylim
  d <- density(x,
    bw = bw,
    adjust = adjust,
    kernel = kernel,
    weights = weights,
    n = n, na.rm = TRUE
  )
  d$y <- yrng[1] + 0.95 * diff(yrng) * d$y / max(d$y, na.rm = TRUE)
  panel.lines(d, ...)
  diag.panel.splom(x, ...) # for labels
}
