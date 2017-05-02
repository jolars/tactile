#' Auto- and Cross- Covariance and -Correlation Function Estimation
#'
#' The following are versions of the functions detailed in
#' [stats::acf()]. Usage is identical except for the plot functionality,
#' which you can read about in [xyplot.acf()].
#'
#' The confidence interval plotted in plot.acf is based on an uncorrelated
#' series and should be treated with appropriate caution. Using `ci.type = "ma"`
#' may potentially be less misleading.
#'
#' @inheritParams stats::acf
#' @param drop_lag0 drop the first lag?
#' @param \dots further arguments to be passed to [xyplot.acf()].
#'
#' @return If `plot = TRUE`, returns and plots a trellis object. Otherwise,
#'   an `acf` object is returned
#'
#' @seealso [xyplot.acf()], [stats::acf()], [lattice::xyplot()]
#'
#' @author Original: Paul Gilbert, Martyn Plummer. Extensive modifications and
#'   univariate case of pacf by B. D. Ripley. Adaptation to lattice by
#'   Johan Larsson.
#' @export
#'
#' @examples
#' library(latticework)
#' autocf(lh)
#' crosscf(mdeaths, fdeaths)
autocf <- function(x,
                   lag.max = NULL,
                   type = c("correlation", "covariance", "partial"),
                   plot = TRUE,
                   na.action = stats::na.fail,
                   demean = TRUE,
                   drop_lag0 = TRUE,
                   ...) {
  out <- stats::acf(x = x,
                    lag.max = lag.max,
                    type = type,
                    plot = FALSE,
                    na.action = na.action,
                    demean = demean)

  out$series <- deparse(substitute(x))

  if (drop_lag0 && match.arg(type) == "correlation") {
    out$acf = out$acf[-1, , , drop = FALSE]
    out$lag = out$lag[-1, , , drop = FALSE]
  }

  if (plot)
    xyplot(out, ...)
  else
    out
}

#' @rdname autocf
#' @export
crosscf <- function(x,
                    y,
                    lag.max = NULL,
                    type = c("correlation", "covariance"),
                    plot = TRUE,
                    na.action = stats::na.fail,
                    ...) {
  out <- stats::ccf(x = x, y = y, lag.max = lag.max, type = type, plot = FALSE,
                    na.action = na.action)
  if (plot)
    xyplot(out, ...)
  else
    out
}

#' @rdname autocf
#' @export
pautocf <- function(x,
                    lag.max = NULL,
                    plot = TRUE,
                    na.action = stats::na.fail,
                    ...) {
  out <- stats::pacf(x = x, lag.max = lag.max, plot = FALSE,
                     na.action = na.action)
  if (plot)
    xyplot(out, ...)
  else
    out
}

#' Plot Autocovariance and Autocorrelation Functions
#'
#' This is a version of [stats::plot.acf()] designed for the special versions
#' of the autocorrelation and autocovariance functions from `latticework`.
#'
#' @inheritParams stats::plot.acf
#' @param data Only provided for method consistency and is ignored.
#' @param \dots Graphical parameters passed on to [lattice::xyplot()].
#'
#' @return Returns and plots a `trellis` object.
#' @seealso [autocf()], [crosscf()], [pautocf()], [lattice::xyplot()],
#'   [stats::plot.acf()], [stats::acf()].
#'
#' @export
#'
#' @examples
#' z  <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
#' autocf(z)
xyplot.acf <- function(x,
                       data = NULL,
                       ci = 0.95,
                       type = "h",
                       xlab = "Lag",
                       ylab = NULL,
                       ylim = NULL,
                       ci.col = "gray50",
                       ci.type = c("white", "ma"),
                       ...) {
  ci.type <- match.arg(ci.type)
  nser <- ncol(x$lag)

  if (nser < 1L)
    stop("x$lag must have at least 1 column")

  if (is.null(ylab))
    ylab <- switch(
      x$type,
      correlation = "ACF",
      covariance = "ACF (cov)",
      partial = "Partial ACF"
    )

  snames <- x$snames

  if (is.null(snames))
    snames <- paste("Series ", if (nser == 1L) x$series else 1L:nser)

  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"

  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }

  clim0 <- if (with.ci) stats::qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

  if (is.null(ylim)) {
    ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
    if (with.ci) ylim <- range(c(-clim0, clim0, ylim))
    if (with.ci.ma) {
      for (i in 1L:nser) {
        clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, i]^2)))
        ylim <- range(c(-clim, clim, ylim))
      }
    }
  }

  # Arrange the data to prepare for plotting
  dd <- data.frame(acf = double(0),
                   lag = double(0),
                   ind1 = character(0),
                   ind2 = character(0))
  for (i in 1L:nser) {
    a <- data.frame(x$acf[, , i], stringsAsFactors = FALSE)
    colnames(a) <- snames
    b <- data.frame(x$lag[, , i], stringsAsFactors = FALSE)
    colnames(b) <- snames
    aa <- utils::stack(a)
    aa[, 3] <- snames[i]
    aa[, 4] <- utils::stack(b)[, 1]
    dd <- rbind(dd, aa)
  }
  colnames(dd) <- c("acf", "ind1", "ind2", "lag")

  # Set up lattice parameters
  ll <- list(...)
  ll$xlab <- xlab
  ll$ylab <- ylab
  ll$data <- dd
  ll$ylim <- grDevices::extendrange(ylim)
  ll$panel <- function(x, y, ...) {
    if (with.ci && ci.type == "white") {
      clim <- clim0
      lattice::panel.abline(h = c(clim, -clim), col = ci.col, lty = 2)
    } else if (with.ci.ma &&
               lattice::current.row() == lattice::current.column()) {
      clim <- clim0 * sqrt(cumsum(c(1, 2 * y[-1]^2)))
      clim <- clim[-length(clim)]
      lattice::panel.lines(x[-1], clim, col = ci.col, lty = 2)
      lattice::panel.lines(x[-1], -clim, col = ci.col, lty = 2)
    }
    lattice::panel.xyplot(x, y, type = "h", ...)
    lattice::panel.abline(h = 0)
  }
  if (nser == 1) {
    ll$x <- stats::formula(acf ~ lag)
  } else if (nser == 2 ) {
    ll$x <- stats::formula(acf ~ lag | ind1)
  } else if (nser > 2) {
    ll$x <- stats::formula(acf ~ lag | ind1 + ind2)
  }
  do.call(lattice::xyplot, ll)
}
