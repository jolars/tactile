#' Diagnostic Plots for ARIMA Models
#'
#' Diagnostic plots modelled after [stats::tsdiag()] with some modifications
#' and corrections of p-values in the Box--Ljung test.
#'
#' @param x A fitted time-series model of class `Arima`.
#' @param data Ignored
#' @param which A sequence of integers between 1 and 4, identifying the
#'   plots to be shown.
#' @param na.action Treatment of `NA`s.
#' @param main Optional titles for the plots. Can also be `TRUE`, in which
#'   case a default list of titles will be added.
#' @param lag.max Number of lags to compute ACF for.
#' @param qq.aspect Aspect of Q-Q plot (see [lattice::qqmath()]).
#' @param gof.lag The maximum number of lags for the Ljung--Box test.
#' @param layout Either a numeric vector with (columns, rows) to use in the call
#'   to [gridExtra::grid.arrange()], or a layout matrix which will then be
#'   passed as the `layout_matrix` in `grid.arrange()`.
#' @param ... Parameters to pass to [xyplot()].
#'
#' @seealso [stats::tsdiag()], [stats::arima()], [lattice::xyplot()],
#'   [gridExtra::grid.arrange()], [stats::Box.test()].
#' @return Plots a lattice plot and returns a `trellis` object.
#' @export
#'
#' @examples
#' fit <- arima(lh, order = c(1, 1, 0))
#' xyplot(fit, layout = c(2, 2))
#' xyplot(fit, which = c(1:2, 4), layout = rbind(c(1, 1), c(2, 3)))
xyplot.Arima <- function(
  x,
  data = NULL,
  which = 1:4,
  lag.max = NULL,
  gof.lag = NULL,
  qq.aspect = "iso",
  na.action = na.pass,
  main = NULL,
  layout = NULL,
  ...
) {
  # Assertions
  stopifnot(length(which) > 0,
            length(which) < 5,
            all(which > 0),
            all(which < 5))

  # Warnings
  if (!is.null(data))
    warning("'data' is ignored.")

  show <- rep.int(FALSE, 4L)
  show[which] <- TRUE

  titles <- rep.int("", 4L)

  if (is.character(main)) {
    stopifnot(length(main) == sum(show))
    titles[which] <- main
  } else if (isTRUE(main)) {
    titles <- c("Standardized residuals over time",
                "Q-Q Plot of Standardized residuals",
                "ACF of residuals",
                "Ljung-Box tests")
  }

  plots <- vector("list", 4L)
  dots <- list(...)
  add.line <- trellis.par.get("add.line")

  na.action <- get_fun(na.action)

  r <- na.action(x$residuals)
  rstd <- r / sqrt(x$sigma2)

  # Standardized residuals
  if (show[1L]) {
    plots[[1L]] <- do.call(xyplot, updateList(list(
      rstd,
      main = main[1L],
      ylab = "Standardized residuals",
      panel = function(...) {
        panel.abline(h = 0L, reference = TRUE)
        panel.xyplot(...)
      }
    ), dots))
  }

  # Q-Q-diagram of standardized residuals
  if (show[2L]) {
    plots[[2L]] <- do.call(qqmath, updateList(list(
      ~ rstd,
      xlab = "Theoretical quantiles",
      ylab = "Standardized residuals",
      main = main[2L],
      aspect = qq.aspect,
      prepanel = prepanel.qqmathline,
      panel = function(...) {
        panel.qqmathci(...)
        panel.qqmathline(...)
        panel.qqmath(...)
      }
    ), dots))
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
    autocor <- acf(r, na.action = na.action, lag.max = lag.max, plot = FALSE)

    plots[[3L]] <- do.call(xyplot, updateList(list(
      x = autocor,
      main = main[3L]
    ), dots))
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

    plots[[4L]] <- do.call(xyplot, updateList(list(
      x = pval ~ seq_along(pval),
      xlab = "Lag",
      ylab = "Ljung-Box p-values",
      main = main[4L],
      ylim = range(c(0, max(pval, na.rm = TRUE) * 1.08, 0.1)),
      panel = function(x, y, ...) {
        panel.abline(h = 0.05, lty = 2)
        panel.xyplot(x, y, ...)
      }
    ), dots))
  }

  grid_wrap(plots, layout = layout)
}
