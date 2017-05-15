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
#' @param lag.max number of lags to compute ACF for
#' @param qq.aspect aspect of Q-Q plot (see [lattice::qqmath()])
#' @inheritParams stats::tsdiag
#' @param layout either a numeric vector with (columns, rows) to use in the call
#'   to [gridExtra::grid.arrange()], or a layout matrix which will then be
#'   passed as the `layout_matrix` in `grid.arrange()`.
#' @param \dots parameters to pass to [xyplot()].
#'
#' @seealso [stats::tsdiag()], [stats::arima()], [lattice::xyplot()],
#'   [gridExtra::grid.arrange()], [stats::Box.test()], [ACF()].
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
                         lag.max = NULL,
                         gof.lag = NULL,
                         qq.aspect = "iso",
                         na.action = na.pass,
                         main = NULL,
                         layout = NULL,
                         ...) {
  show <- rep.int(FALSE, 4L)
  show[which] <- TRUE
  plots <- vector("list", 4L)

  if (!is.null(main))
    stopifnot(length(main) == sum(show))

  na.action <- getFunctionOrName(na.action)

  r <- na.action(x$residuals)
  rstd <- r / sqrt(x$sigma2)

  # Standardized residuals
  if (show[1L]) {
    plots[[1L]] <- xyplot(
      rstd,
      ylab = "Standardized residuals",
      ...,
      panel = function(...) {
        panel.abline(h = 0L, col = trellis.par.get()$add.line$col)
        panel.xyplot(...)
      }
    )
  }

  # Q-Q-diagram of standardized residuals
  if (show[2L]) {
    plots[[2L]] <- qqmath(
      ~ rstd,
      aspect = qq.aspect,
      ...,
      panel = function(...) {
        panel.qqmathci(...)
        panel.qqmathline(...)
        panel.qqmath(...)
      }
    )
  }

  if (any(show[3L:4L])) {
    df <- sum(x$arma[c(1, 3, 4, 7)], na.rm = TRUE)
    period <- x$arma[5]
  }

  # ACF of residuals
  if (show[3L]) {
    if (is.null(lag.max)) {
      lag.max <- if (period < 6) 20 else 3 * period
      if (lag.max <= df + 8)
        lag.max <- df + 8
    }

    plots[[3L]] <- ACF(r, na.action = na.action, lag.max = lag.max, ...)
  }

  # Box-Ljung p.tests
  if (show[4L]) {
    if (is.null(gof.lag)) {
      gof.lag <- if (period < 6) 20 else 3 * period
      if (gof.lag <= df + 8)
        gof.lag <- df + 8
    } else if (gof.lag < df) {
      stop("'gof.lag' cannot be < df (p + q + P + Q)")
    }

    pval <- double(gof.lag)
    pval[] <- NA
    for (i in (df + 1):gof.lag)
      pval[i] <- Box.test(r, i, "Ljung-Box", df)$p.value

    ll <- list(
      x = pval ~ seq_along(pval),
      xlab = "Lag",
      ylab = "Ljung-Box p-values",
      ylim = range(c(0, max(pval, na.rm = TRUE) * 1.08, 0.1)),
      panel = function(x, y, ...) {
        panel.abline(h = 0.05, lty = 2L, col = trellis.par.get()$add.line$col)
        panel.xyplot(x, y, ...)
      }
    )

    plots[[4L]] <- do.call(xyplot, updateList(ll, list(...)))
  }

  grid_wrap(plots, layout = layout)
}
