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
#' @param drop_lag0 Set to `TRUE` to drop the lag at 0.
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
                     drop_lag0 = TRUE,
                     ...) {
  if (is.null(lag.max)) {
    p <- length(ar)
    q <- length(ma)
    if (!p && !q)
      stop("The model is empty.")
    lag.max <- max(p, q + 1)
  }

  y <- ARMAacf(ar = ar, ma = ma, lag.max = lag.max, pacf = pacf)
  if (pacf) {
    x <- seq_along(y)
  } else if (drop_lag0) {
    y <- y[-1L]
    x <- seq_along(y)
  } else {
    x <- seq_along(y) - 1
  }

  yrng <- range(y, na.rm = TRUE)
  if (yrng[1] > 0) yrng[1] <- 0
  if (yrng[2] < 0) yrng[2] <- 0
  ylim <- extendrange(r = yrng)

  xrng <- range(x)
  if (xrng[2] < 10) xrng[2] <- 10
  xlim <- extendrange(r = xrng)

  if (plot)
    xyplot(
      y ~ x,
      ylab = if (pacf) "Partial ACF" else "ACF",
      xlab = "Lag",
      ylim = ylim,
      xlim = xlim,
      ...,
      panel = function(x, y, ...) {
        panel.xyplot(x, y, type = "h", ...)
        panel.abline(h = 0)
      }
    )
  else
    y
}
