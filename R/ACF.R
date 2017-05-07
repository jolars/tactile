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
#' @name ACF
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
#'
#' @examples
#' ACF(lh)
#' PACF(lh)
#' CCF(mdeaths, fdeaths)
NULL

#' @describeIn ACF Autocorrelation and autocovariance
#' @export
ACF <- function(x,
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

  if (drop_lag0) {
    out$acf = out$acf[-1L, , , drop = FALSE]
    out$lag = out$lag[-1L, , , drop = FALSE]
  }

  if (plot)
    xyplot(x = out, ...)
  else
    out
}

#' @describeIn ACF Crosscorrelation and crosscovariance
#' @export
CCF <- function(x,
                y,
                lag.max = NULL,
                type = c("correlation", "covariance"),
                plot = TRUE,
                na.action = stats::na.fail,
                ...) {
  out <- stats::ccf(x = x, y = y, lag.max = lag.max, type = type, plot = FALSE,
                    na.action = na.action)

  out$series <- deparse(substitute(x))

  if (plot)
    xyplot(x = out, ...)
  else
    out
}

#' @describeIn ACF Partial autocorrelation and autocovariance
#' @export
PACF <- function(x,
                 lag.max = NULL,
                 plot = TRUE,
                 na.action = stats::na.fail,
                 ...) {
  out <- stats::pacf(x = x, lag.max = lag.max, plot = FALSE,
                     na.action = na.action)

  out$series <- deparse(substitute(x))

  if (plot)
    xyplot(x = out, ...)
  else
    out
}

#' Plot Autocovariance and Autocorrelation Functions
#'
#' This is a version of [stats::plot.acf()] designed for the special versions
#' of the autocorrelation and autocovariance functions from `latticework`.
#'
#' @inheritParams stats::plot.acf
#' @param data only provided for method consistency and is ignored.
#' @param \dots graphical parameters passed on to [lattice::xyplot()].
#'
#' @return Returns and plots a `trellis` object.
#' @seealso [ACF()], [CCF()], [PACF()], [lattice::xyplot()],
#'   [stats::plot.acf()], [stats::acf()].
#'
#' @export
#'
#' @examples
#' z <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
#' ACF(z, ci.col = "black")
xyplot.acf <- function(x,
                       data = NULL,
                       ci = 0.95,
                       ci.col = "gray50",
                       ci.type = c("white", "ma"),
                       ...) {
  ci.type <- match.arg(ci.type)
  nser <- ncol(x$lag)

  if (nser < 1L)
    stop("x$lag must have at least 1 column")

  if (is.null(snames <- x$snames))
    snames <- paste("Series ", if (nser == 1L) x$series else 1L:nser)

  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"

  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }

  clim0 <- if (with.ci) stats::qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

  ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
  if (with.ci)
    ylim <- range(c(-clim0, clim0, ylim))
  if (with.ci.ma) {
    for (i in 1L:nser) {
      clim <- clim0 * sqrt(cumsum(c(1L, 2L * x$acf[-1L, i, i]^2L)))
      ylim <- range(c(-clim, clim, ylim))
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
    aa[, 3L] <- snames[i]
    aa[, 4L] <- utils::stack(b)[, 1L]
    dd <- rbind(dd, aa)
  }
  colnames(dd) <- c("acf", "ind1", "ind2", "lag")

  # Set up lattice parameters
  plot_pars <- list(
    x = if (nser == 1L) {
      acf ~ lag
    } else if (nser == 2L ) {
      acf ~ lag | ind1
    } else if (nser > 2L) {
      acf ~ lag | ind1 + ind2
    },
    xlab = "Lag",
    ylab = switch(x$type,
                  correlation = "ACF",
                  covariance = "ACF (cov)",
                  partial = "Partial ACF"),
    data = dd,
    ylim = grDevices::extendrange(ylim),
    panel = function(x, y, ...) {
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
  )

  plot_pars <- utils::modifyList(plot_pars, list(...))

  do.call(lattice::xyplot, plot_pars)
}
