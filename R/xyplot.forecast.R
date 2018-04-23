#' Plot Forecasts with Trellis Graphics
#'
#' Plot forecasts from [forecast::forecast()]. It is built mostly to resemble
#' the [forecast::autoplot.forecast()] and [forecast::plot.forecast()]
#' functions, but in addition tries to plot the predictions on the original
#' scale.
#'
#' This function requires the **zoo** package.
#'
#' @param x An object of class `forecast`.
#' @param data Data of observations left out of the model fit, usually
#'   "future" observations.
#' @param ci Plot confidence intervals for the predictions.
#' @param ci_levels The prediction levels to plot as a subset of those
#'   forecasted in `x`.
#' @param ci_alpha Fill alpha for the confidence interval.
#' @param ... Arguments passed on to [lattice::panel.xyplot()].
#' @param ci_key Set to `TRUE` to draw a key automatically or provide a list
#'   (if `length(ci_levels)` > 5 should work with [lattice::draw.colorkey()] and
#'   otherwise with [lattice::draw.key()])
#' @param ci_pal Color palette for the confidence bands.
#'
#' @inherit lattice::qqmath return
#' @export
#'
#' @seealso [lattice::panel.xyplot()], [forecast::forecast()], [lattice::xyplot.ts()].
#'
#' @examples
#' if (require(forecast)) {
#'   train <- window(USAccDeaths, c(1973, 1), c(1977, 12))
#'   test <- window(USAccDeaths, c(1978, 1), c(1978, 12))
#'   fit <- arima(train, order = c(0, 1, 1),
#'                seasonal = list(order = c(0, 1, 1)))
#'   fcast1 <- forecast(fit, 12)
#'   xyplot(fcast1, test, grid = TRUE, auto.key = list(corner = c(0, 0.99)),
#'          ci_key = list(title = "PI Level"))
#'
#'   # A fan plot
#'   fcast2 <- forecast(fit, 12, level = seq(0, 95, 10))
#'   xyplot(fcast2, test, ci_pal = heat.colors(100))
#' }

xyplot.forecast <- function(
  x,
  data = NULL,
  ci = TRUE,
  ci_levels = x$level,
  ci_key = ci,
  ci_pal = hcl(0, 0, 45:100),
  ci_alpha = trellis.par.get("regions")$alpha,
  ...
) {
  dots <- list(...)
  if (is.null(x$lower) | is.null(x$upper) | is.null(x$level))
    ci <- FALSE
  else if (!is.finite(max(x$upper)))
    ci <- FALSE

  # Required packages
  require_pkg("zoo")

  actual <- zoo::as.zoo(x$x)
  pred <- zoo::as.zoo(drop(x$mean))
  pred <- c(tail(actual, 1), pred)

  # Which levels should we plot?
  which_levels <- x$level %in% ci_levels

  # Drop levels not in x$level
  ci_levels <- ci_levels[ci_levels %in% x$level]
  if (any(!(ci_levels %in% x$level)))
    warning("Some (or all) of the required prediction intervals were not found in 'x'.")

  if (!is.null(data))
    actual <- c(actual, zoo::as.zoo(data))

  fills <- rgb(grDevices::colorRamp(ci_pal)(ci_levels / 100),
               maxColorValue = 255)

  dd <- merge(Actual = actual, Fitted = pred)
  upr <- zoo::zoo(x$upper[, which_levels], zoo::index(x$mean))
  lwr <- zoo::zoo(x$lower[, which_levels], zoo::index(x$mean))

  out <- do.call(xyplot, updateList(list(
    dd,
    superpose = TRUE,
    lty = 1:2,
    ylim = extendrange(c(zoo::coredata(dd), zoo::coredata(upr),
                         zoo::coredata(lwr))),
    panel = function(x, grid = FALSE, ...) {
      if (grid)
        panel.grid(h = -1, v = -1)
      if (ci) {
        xx <- c(time(upr), rev(time(upr)))
        yu <- zoo::coredata(upr)
        yl <- zoo::coredata(lwr)
        if (NCOL(upr) == 1) {
          panel.polygon(x = xx, y = c(yu, rev(yl)),
                        col = fills, alpha = ci_alpha,
                        border = "transparent")
        } else {
          for (i in 1:NCOL(upr)) {
            if (i == 1) {
              panel.polygon(x = xx, y = c(yu[, 1], rev(yl[, 1])),
                            col = fills[i], alpha = ci_alpha,
                            border = "transparent")
            } else {
              panel.polygon(x = xx, y = c(yu[, i - 1], rev(yu[, i])),
                            col = fills[i], alpha = ci_alpha,
                            border = "transparent")
              panel.polygon(x = xx, y = c(yl[, i - 1], rev(yl[, i])),
                            col = fills[i], alpha = ci_alpha,
                            border = "transparent")
            }
          }
        }
      }
      panel.xyplot(x, grid = FALSE, ...)
    }
  ), dots))

  # Setup a key?
  if (length(ci_levels) > 5) {
    out$legend <- setup_key(
      out$legend,
      ci_key,
      list(col = adjustcolor(fills, alpha.f = ci_alpha),
           at = c(ci_levels / 100, 1)),
      draw.colorkey
    )
  } else if (length(ci_levels) > 1) {
    out$legend <- setup_key(
      out$legend,
      ci_key,
      list(text = list(paste0(ci_levels, "%")),
           rectangles = list(col = fills, alpha = ci_alpha)),
      draw.key
    )
  }
  out
}


