#' Diagnostic Plots for Time-Series Fits with lattice
#'
#' Diagnostic plots modelled after [stats::tsdiag()] with some modifications
#' and corrections of p-values in the Box--Ljung test.
#'
#' @param x a fitted time-series model
#' @param data ignored (only added for method consistency)
#' @param which which plots should be plotted?
#' @param na.action what to do about na.values when computing ACF?
#' @param main optional titles for the plots
#' @inheritParams stats::tsdiag
#' @param layout either a numeric vector with (columns, rows) to use in the call
#'   to [gridExtra::grid.arrange()], or a layout matrix which will then be
#'   passed as the `layout_matrix` in `grid.arrange()`.
#' @param par.settings graphical parameters passed on to [lattice::xyplot()].
#' @param \dots other parameters to pass to [lattice::xyplot()].
#'
#' @seealso [stats::tsdiag()], [stats::arima()], [lattice::xyplot()],
#'   [gridExtra::grid.arrange()], [stats::Box.test()], [autocf()].
#' @return Plots a lattice plot and returns a `trellis` object.
#' @export
#'
#' @examples
#' fit <- arima(lh, order = c(1, 1, 0))
#' xyplot(fit, layout = c(2, 2))
#' xyplot(fit, which = c(1:2, 4), layout = rbind(c(1, 1), c(2, 3)))
xyplot.Arima <- function(x,
                         data = NULL,
                         which = 1:4,
                         gof.lag = 10,
                         na.action = stats::na.pass,
                         main = NULL,
                         layout = NULL,
                         par.settings = list(
                           layout.heights = list(top.padding = 0)
                         ),
                         ...) {
  show <- rep(FALSE, 4)
  show[which] <- TRUE
  plots <- vector("list", 4)

  if (!is.null(main))
    stopifnot(length(main) == sum(show))

  r <- stats::residuals(x) / sqrt(x$sigma2)

  # Standardized residuals
  if (show[1L]) {
    plots[[1L]] <- lattice::xyplot(
      r ~ seq_along(r),
      ylab = "Standardized residuals",
      xlab = "Time",
      par.settings = par.settings,
      ...,
      panel = function(x, y, ...) {
        lattice::panel.abline(h = 0, col = "gray50")
        lattice::panel.xyplot(x, y, type = "l", ...)
      }
    )
  }

  # Q-Q-diagram of standardized residuals
  if (show[2L]) {
    plots[[2L]] <- lattice::qqmath(
      ~ r,
      ylab = "Standardized residuals",
      xlab = "Time",
      par.settings = par.settings,
      ...,
      panel = function(x, y, ...) {
        lattice::panel.qqmathline(x, col = "gray50")
        lattice::panel.qqmath(x, ...)
      }
    )
  }

  # ACF of residuals
  if (show[3L])
    plots[[3L]] <- autocf(x$residuals,
                          na.action = na.action,
                          par.settings = par.settings,
                          ...)

  # Box-Ljung p.tests
  if (show[4L]) {
    df <- length(c(x$model$phi, x$model$theta))

    if (gof.lag < df)
      stop("'gof.lag' cannot be < df (p +q)")

    along <- seq.int(1L + df, gof.lag)

    pval <- vapply(along, function(i) {
      stats::Box.test(r, i, fitdf = df, type = "Ljung-Box")$p.value
    }, FUN.VALUE = numeric(1))

    plots[[4L]] <- lattice::xyplot(
      pval ~ along,
      xlab = "Lag",
      ylab = "Box-Ljung p-values",
      ylim = c(0, max(pval) * 1.08),
      par.settings = par.settings,
      ...,
      panel = function(x, y, ...) {
        lattice::panel.abline(h = 0.05, lty = 2, col = "gray50")
        lattice::panel.xyplot(x, y, ...)
      }
    )
  }

  grid_wrap(plots, layout = layout)
}
