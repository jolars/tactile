#' Title
#'
#' @param x
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
xyplot.forecast <- function(x,
                            data = NULL,
                            include = NULL,
                            pi = 95,
                            pi_col = "grey75",
                            ...)
{
  object <- x

  if (is.null(object$lower) | is.null(object$upper) | is.null(object$level))
    pi <- FALSE
  else if (!is.finite(max(object$upper)))
    pi <- FALSE

  if (!is.null(object$model$terms) && !is.null(object$model$model)) {
    mt <- object$model$terms
    yvar <- if (!is.null(object$series)) object$series else deparse(mt[[2]])
    xvar <- attr(mt, "term.labels")
    vars <- c(yvar = yvar, xvar = xvar)
    data <- object$model$model
    colnames(data) <- names(vars)[match(colnames(data), vars)]
    if (!is.null(object$model$lambda))
      data$yvar <- forecast::InvBoxCox(data$yvar, object$model$lambda)
  } else {
    if (!is.null(object$x))
      data <- data.frame(yvar = c(object$x))
    else if (!is.null(object$residuals) && !is.null(object$fitted))
      data <- data.frame(yvar = c(object$residuals + object$fitted))
    else
      stop("Could not find data")

    if (!is.null(object$series))
      vars <- c(yvar = object$series)
    else if (!is.null(object$model$call)) {
      vars <- c(yvar = deparse(object$model$call$y))
      if (vars == "object")
        vars <- c(yvar = "y")
    } else
      vars <- c(yvar = "y")
  }

  if (!is.element("ts", class(object$mean))) {
    if (length(xvar) > 1)
      stop("Forecast plot for regression models only available for a single predictor")
    if (NCOL(object$newdata) == 1)
      colnames(object$newdata) <- xvar

    predicted <- data.frame(xpred = object$newdata, ypred = object$mean)

    if (pi) {
      levels <- NROW(object$level)
      interval <- data.frame(
        xpred = rep(object$newdata[[1]], levels),
        lower = c(object$lower),
        upper = c(object$upper),
        level = object$level
      )
      interval <- interval[order(interval$level, decreasing = TRUE), ]
      interval <- interval[interval$level == pi, ]
    }

    p <- list(
      x = yvar ~ xvar,
      data = data,
      ylab = vars["yvar"],
      xlab = vars["xvar"],
      xlim = extendrange(c(data$datetime, predicted$xvar)),
      ylim = extendrange(
        c(data$yvar,
          if (pi) c(interval$upper, interval$lower) else (predicted$ypred))
      ),
      type = "l",
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        if (pi) {
          panel.lines(x = interval$xpred, y = interval$ypred, lty = 2)
          panel.polygon(
            c(interval$datetime, rev(interval$datetime)),
            c(interval$upper, rev(interval$lower)),
            alpha = 0.1,
            col = pi_col,
            border = "transparent"
          )
        }
        panel.points(predicted$xpred, predicted$ypred)
        coef <- data.frame(int = 0, m = 0)
        i <- match("(Intercept)", names(object$model$coefficients))
        if (i != 0) {
          coef$int <- object$model$coefficients[i]
          if (NROW(object$model$coefficients) == 2) {
            coef$m <- object$model$coefficients[-i]
          }
        }
        else {
          if (NROW(object$model$coefficients) == 1) {
            coef$m <- object$model$coefficients
          }
        }
        panel.abline(coef$int, coef$m)
      }
    )
  } else {
    if (!is.null(time(object$x))) {
      timex <- time(object$x)
    }
    else if (!is.null(time(object$model$residuals))) {
      timex <- time(object$model$residuals)
    }
    data <- data.frame(yvar = as.numeric(data$yvar),
                       datetime = as.numeric(timex))
    if (!is.null(include))
      data <- tail(data, include)

    predicted <- data.frame(datetime = time(object$mean), ypred = object$mean)

    if (pi) {
      levels <- NROW(object$level)
      interval <- data.frame(
        datetime = rep(predicted$datetime, levels),
        lower = c(object$lower),
        upper = c(object$upper),
        level = rep(object$level, each = NROW(object$mean))
      )
      interval <- interval[order(interval$level, decreasing = TRUE), ]
      interval <- interval[interval$level == pi, ]
    }

    p <- list(
      x = yvar ~ datetime,
      data = data,
      ylab = vars["yvar"],
      xlab = "Time",
      type = "l",
      xlim = extendrange(c(data$datetime, predicted$datetime)),
      ylim = extendrange(c(data$yvar,
                           if (pi) c(interval$upper, interval$lower)
                           else (predicted$ypred))
      ),
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        if (pi)
          panel.polygon(
            c(interval$datetime, rev(interval$datetime)),
            c(interval$upper, rev(interval$lower)),
            alpha = 0.5,
            col = pi_col,
            border = "transparent",
            ...
          )
        panel.lines(predicted$datetime, predicted$ypred, lty = 2, ...)
      }
    )
  }
  do.call(xyplot, updateList(p, list(...)))
}
