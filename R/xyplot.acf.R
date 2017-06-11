#' Plot Autocovariance and Autocorrelation Functions
#'
#' This is a version of [stats::plot.acf()].
#'
#' @inheritParams stats::plot.acf
#' @param data only provided for method consistency and is ignored.
#' @param \dots graphical parameters passed on to [lattice::xyplot()].
#'
#' @return Returns and plots a `trellis` object.
#' @seealso [lattice::xyplot()], [stats::plot.acf()], [stats::acf()].
#'
#' @export
#'
#' @examples
#' z <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
#' xyplot(acf(z, ci.col = "black"))
xyplot.acf <- function(x,
                       data = NULL,
                       ci = 0.95,
                       ci.col = "gray50",
                       ci.type = c("white", "ma"),
                       ...) {
  ci.type <- match.arg(ci.type)
  nser <- NCOL(x$lag)

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

  clim0 <- if (with.ci) qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

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
    aa <- stack(a)
    aa[, 3L] <- snames[i]
    aa[, 4L] <- stack(b)[, 1L]
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
        panel.abline(h = c(clim0, -clim0), reference = TRUE, lty = 2L)
      } else if (with.ci.ma && current.row() == current.column()) {
        clim <- clim0 * sqrt(cumsum(c(1, 2 * y[-1]^2)))
        clim <- clim[-length(clim)]
        panel.lines(x[-1], clim, col = ci.col, lty = 2)
        panel.lines(x[-1], -clim, col = ci.col, lty = 2)
      }
      panel.xyplot(x, y, type = "h", ...)
      panel.abline(h = 0)
    }
  )

  do.call(xyplot, updateList(plot_pars, list(...)))
}
