#' Compute (and Plot) Theoretical ACF for an ARMA Process
#'
#' A wrapper around [stats::ARMAacf()] that optionally plots the result
#' using a call to [lattice::xyplot()].
#'
#' @inherit stats::ARMAacf details
#' @inherit stats::ARMAacf references
#'
#' @inheritParams stats::ARMAacf
#' @param plot Set to `TRUE` to plot the resulting ACF via [lattice::xyplot]
#' @param \dots Graphical parameters passed onto [lattice::xyplot()].
#'
#' @return A vector of (partial) autocorrelations, named by the lags or, if
#'   `plot = TRUE`, a `trellis` object.
#' @seealso [lattice::xyplot()], [stats::arima()], [stats::ARMAtoMA()],
#'   [stats::acf2AR()].
#'
#' @export
#'
#' @examples
#' arma_acf(ar = c(1.0, -0.25), ma = 1.0, lag.max = 10)
#' arma_acf(ar = -0.8, lag.max = 15, plot = TRUE)
arma_acf <- function(ar = numeric(),
                     ma = numeric(),
                     lag.max = NULL,
                     pacf = FALSE,
                     plot = FALSE,
                     ...) {
  if (is.null(lag.max)) {
    p <- length(ar)
    q <- length(ma)
    if (!p && !q)
      stop("empty model supplied")
    lag.max <- max(p, q + 1)
  }

  y <- stats::ARMAacf(ar = ar, ma = ma, lag.max = lag.max, pacf = pacf)
  x <- as.integer(names(y))

  yrng <- range(y, na.rm = TRUE)
  if (yrng[1] > 0) yrng[1] <- 0
  if (yrng[2] < 0) yrng[2] <- 0
  ylim <- grDevices::extendrange(r = yrng)

  xrng <- range(x)
  if (xrng[2] < 10) xrng[2] <- 10
  xlim <- grDevices::extendrange(r = xrng)

  if (plot)
    lattice::xyplot(
      y ~ x,
      type = "h",
      ylab = if (pacf) "PACF" else "ACF",
      xlab = "Lag",
      ylim = ylim,
      xlim = xlim,
      ...,
      panel = function(x, y, ...) {
        lattice::panel.abline(h = 0)
        lattice::panel.xyplot(x, y, ...)
      }
    )
  else
    y
}
