#' Panel function for confidence interval
#'
#' @inheritParams lattice::panel.xyplot
#' @param lower lower confidence limits
#' @param upper upper confidence limits
#' @param subscripts see [lattice::xyplot()]
#' @param alpha opacity for the fill
#' @param col line color
#' @param fill fill color
#' @param lty line type
#' @param lwd line width
#' @param col.line line color. Supersedes `col` if both are specified.
#'
#' @export
#'
#' @examples
#' mod <- lm(Petal.Width ~ Petal.Length * Species, data = iris)
#' newdat <- expand.grid(
#'   Petal.Length = seq(1, 7, by = 0.1),
#'   Species = c("setosa", "versicolor", "virginica")
#' )
#' pred <- predict(mod, newdat, interval = "confidence")
#' dd <- cbind(newdat, pred)
#'
#' xyplot(
#'   fit ~ Petal.Length,
#'   groups = Species, data = dd,
#'   prepanel = prepanel.ci, auto.key = list(lines = TRUE, points = FALSE),
#'   ylab = "Petal Width",
#'   xlab = "Petal Length",
#'   lower = dd$lwr, upper = dd$upr, type = "l",
#'   panel = function(...) {
#'     panel.ci(..., alpha = 0.15, grid = TRUE)
#'     panel.xyplot(...)
#'   }
#' )
#'
panel.ci <- function(
    x,
    y,
    lower,
    upper,
    groups = NULL,
    subscripts,
    col,
    fill = if (is.null(groups)) plot.line$col else superpose.line$col,
    alpha = 0.15,
    lty = 0,
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
    grid = FALSE,
    ...,
    col.line = if (is.null(groups)) plot.line$col else superpose.line$col) {
  plot.line <- trellis.par.get("plot.line")
  superpose.line <- trellis.par.get("superpose.line")

  dots <- list(...)

  if (!missing(col)) {
    if (missing(col.line)) {
      col.line <- col
    }
  }

  # Borrowed from panel.xyplot()
  if (!identical(grid, FALSE)) {
    if (!is.list(grid)) {
      grid <- switch(as.character(grid),
        "TRUE" = list(h = -1, v = -1, x = x, y = y),
        h = list(h = -1, v = 0, y = y),
        v = list(h = 0, v = -1, x = x),
        list(h = 0, v = 0)
      )
    }
    do.call(lattice::panel.grid, grid)
  }

  nobs <- sum(!is.na(y))
  if (!is.null(groups)) {
    do.call(panel.superpose, updateList(list(
      x = x,
      y = y,
      lower = lower,
      upper = upper,
      groups = groups,
      subscripts = subscripts,
      panel.groups = panel.ci,
      alpha = alpha,
      col.line = col.line,
      fill = fill,
      lty = lty,
      lwd = lwd
    ), dots))
  } else if (nobs > 0) {
    lower <- lower[subscripts]
    upper <- upper[subscripts]
    ord <- order(x)
    x <- sort(x)

    # Confidence bands
    do.call(
      panel.polygon,
      updateList(list(
        x = c(x, rev(x)),
        y = c(upper[ord], rev(lower[ord])),
        alpha = alpha,
        col = fill,
        border = "transparent",
        lty = 0,
        lwd = 0,
        identifier = "ci"
      ), dots)
    )

    # Lower bounds
    panel.lines(
      x = x,
      y = lower[ord],
      alpha = alpha,
      col = col.line,
      lty = lty,
      lwd = lwd,
      identifier = "ci"
    )

    # Upper bounds
    panel.lines(
      x = x,
      y = upper[ord],
      alpha = alpha,
      col = col.line,
      lty = lty,
      lwd = lwd,
      identifier = "ci"
    )
  } else {
    return()
  }
}

#' Prepanel for ciplot
#'
#' @inheritParams lattice::prepanel.default.xyplot
#' @inheritParams panel.ci
#' @export
#'
#' @examples
#' mod <- lm(Petal.Width ~ Petal.Length * Species, data = iris)
#' newdat <- expand.grid(
#'   Petal.Length = seq(1, 7, by = 0.1),
#'   Species = c("setosa", "versicolor", "virginica")
#' )
#' pred <- predict(mod, newdat, interval = "confidence")
#' dd <- cbind(newdat, pred)
#'
#' xyplot(
#'   fit ~ Petal.Length,
#'   groups = Species, data = dd,
#'   prepanel = prepanel.ci,
#'   ylab = "Petal Width",
#'   xlab = "Petal Length",
#'   lower = dd$lwr, upper = dd$upr, alpha = 0.3,
#'   panel = function(...) {
#'     panel.ci(..., grid = TRUE)
#'     panel.xyplot(type = "l", ...)
#'   }
#' )
#'
prepanel.ci <- function(
    x,
    y,
    lower,
    upper,
    subscripts,
    groups = NULL,
    ...) {
  if (any(!is.na(x)) && any(!is.na(y))) {
    ord <- order(as.numeric(x))
    if (!is.null(groups)) {
      gg <- groups[subscripts]
      dx <- unlist(lapply(split(as.numeric(x)[ord], gg[ord]), diff))
      dy <- unlist(lapply(split(as.numeric(y)[ord], gg[ord]), diff))
    } else {
      dx <- diff(as.numeric(x[ord]))
      dy <- diff(as.numeric(y[ord]))
    }
    list(
      xlim = scale.limits(x),
      ylim = scale.limits(c(lower, upper)),
      dx = dx,
      dy = dy,
      xat = if (is.factor(x)) sort(unique(as.numeric(x))) else NULL,
      yat = if (is.factor(y)) sort(unique(as.numeric(y))) else NULL
    )
  } else {
    prepanel.null()
  }
}
