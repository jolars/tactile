#' Plot Autocovariance and Autocorrelation Functions
#'
#' This is a version of [stats::plot.acf()].
#'
#' @param x An 'acf' object.
#' @param ci Confidence level.
#' @param ci_type Type of confidence level.
#' @param ci_col Line color for the confidence levels.
#' @param ci_lty Line type for the confidence levels.
#' @param ... Arguments passed on to [lattice::xyplot()].
#' @param data Ignored
#'
#' @author Original by Brian Ripley.
#'
#' @return Returns and plots a `trellis` object.
#' @seealso [lattice::xyplot()], [stats::plot.acf()], [stats::acf()].
#'
#' @export
#'
#' @examples
#' z <- ts(matrix(rnorm(400), 100, 4), start = c(1961, 1), frequency = 12)
#' xyplot(acf(z))
xyplot.acf <- function(
  x,
  data = NULL,
  ci = 0.95,
  ci_type = c("white", "ma"),
  ci_col = trellis.par.get("add.line")$col,
  ci_lty = 2,
  ...
) {
  ci_type <- match.arg(ci_type)
  nser <- NCOL(x$lag)

  if (nser < 1L) {
    stop("x$lag must have at least 1 column")
  }

  if (is.null(snames <- x$snames)) {
    snames <- paste("Series ", if (nser == 1L) x$series else 1L:nser)
  }

  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci_type == "ma" && x$type == "correlation"

  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci_type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }

  clim0 <- if (with.ci) qnorm((1 + ci) / 2) / sqrt(x$n.used) else c(0, 0)

  ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
  if (with.ci) {
    ylim <- extendrange(c(-clim0, clim0, ylim))
  }
  if (with.ci.ma) {
    for (i in 1L:nser) {
      clim <- clim0 * sqrt(cumsum(c(1L, 2L * x$acf[-1L, i, i]^2L)))
      ylim <- extendrange(c(-clim, clim, ylim))
    }
  }

  # Arrange the data to prepare for plotting
  dd <- data.frame(
    acf = double(0),
    lag = double(0),
    ind1 = character(0),
    ind2 = character(0)
  )
  for (i in 1L:nser) {
    a <- data.frame(x$acf[,, i], stringsAsFactors = FALSE)
    colnames(a) <- snames
    b <- data.frame(x$lag[,, i], stringsAsFactors = FALSE)
    colnames(b) <- snames
    aa <- stack(a)
    aa[, 3L] <- snames[i]
    aa[, 4L] <- stack(b)[, 1L]
    dd <- rbind(dd, aa)
  }
  colnames(dd) <- c("acf", "ind1", "ind2", "lag")

  do.call(
    xyplot,
    updateList(
      list(
        x = if (nser == 1L) {
          acf ~ lag
        } else if (nser == 2L) {
          acf ~ lag | ind1
        } else if (nser > 2L) {
          acf ~ lag | ind1 + ind2
        },
        xlab = "Lag",
        ylab = switch(
          x$type,
          correlation = "ACF",
          covariance = "ACF (cov)",
          partial = "Partial ACF"
        ),
        data = dd,
        ylim = ylim,
        panel = function(x, y, ...) {
          if (with.ci && ci_type == "white") {
            panel.abline(h = c(clim0, -clim0), col = ci_col, lty = ci_lty)
          } else if (with.ci.ma && current.row() == current.column()) {
            clim <- clim0 * sqrt(cumsum(c(1, 2 * y[-1]^2)))
            clim <- clim[-length(clim)]
            panel.lines(x[-1], clim, col = ci_col, lty = ci_lty)
            panel.lines(x[-1], -clim, col = ci_col, lty = ci_lty)
          }
          panel.xyplot(x, y, type = "h", ...)
          panel.abline(h = 0)
        }
      ),
      list(...)
    )
  )
}
