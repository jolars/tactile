#' Title
#'
#' @param x
#' @param data
#' @param include
#' @param pi
#' @param pi_col
#' @param ...
#'
#' @inherit lattice::qqmath return
#' @export
#'
#' @examples
xyplot.forecast <- function(
    x,
    data = NULL,
    interval = TRUE,
    interval_col = trellis.par.get("reference.line")$col,
    interval_alpha = 0.5,
    ...
  ) {
  if (is.null(x$lower) | is.null(x$upper) | is.null(x$level))
    interval <- FALSE
  else if (!is.finite(max(x$upper)))
    interval <- FALSE

  actual <- as.zoo(x$x)
  pred <- as.zoo(drop(x$mean))
  pred <- c(tail(actual, 1), pred)

  if (!is.null(data)) {
    actual <- c(actual, as.zoo(data))
  }

  dd <- merge(Actual = actual, Forecast = pred)
  upr <- as.zoo(x$upper)
  lwr <- as.zoo(x$lower)

  ll <- list(
    dd,
    superpose = TRUE,
    lty = 1:2,
    ylim = extendrange(c(coredata(dd), coredata(upr), coredata(lwr))),
    panel = function(x, grid = FALSE, ...) {
      if (grid)
        panel.grid(h = -1, v = -1)
      if (interval) {
        for (i in 1:NCOL(upr)) {
          if (NCOL(upr) > 1) {
            u <- upr[, i]
            l <- lwr[, i]
          } else {
            u <- upr
            l <- lwr
          }
          yy <- c(coredata(u), rev(coredata(l)))
          xx <- c(time(u), rev(time(l)))

          panel.polygon(xx,
                        yy,
                        border = "transparent",
                        col = interval_col,
                        alpha = interval_alpha)
        }
      }
      panel.xyplot(x, grid = FALSE, ...)
    }
  )
  do.call(xyplot, updateList(ll, list(...)))
}
