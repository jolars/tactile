#' Q-Q Diagram Confidence Intervals Panels
#'
#' Panel function to go along with [lattice::qqmath()] and
#' [lattice::panel.qqmathline()]. Adds filled confidence bands to the Q-Q-plot.
#'
#' The function tries to figure out the density function counterpart to
#' that provided in the argument `distribution` by regular expressions.
#'
#' @inheritParams lattice::panel.qqmathline
#' @param ci Confidence level
#' @param alpha Alpha level for the color fill
#' @param \dots Arguments passed to [lattice::panel.superpose()] and
#'   [lattice::panel.polygon()]
#' @param col Color fill for the confidence bands.
#' @param col.line Color fill for the confidence bands. Is used internally
#'   by [lattice::panel.superpose()] and should generally not be changed.
#'
#' @author Johan Larsson.
#' @seealso [lattice::panel.qqmathline()], [lattice::qqmath()], and
#'   [lattice::panel.qqmath()].
#'
#' @return Augments a trellis plot panel, such as that
#'   created by [lattice::qqmath()], with confidence levels.
#' @export
#'
#' @examples
#' qqmath(~ height | voice.part, aspect = "xy", data = singer,
#'        prepanel = prepanel.qqmathline,
#'        panel = function(x, ...) {
#'          panel.qqmathci(x, ...)
#'          panel.qqmathline(x, ...)
#'          panel.qqmath(x, ...)
#'        })
panel.qqmathci <- function(
  x,
  y = x,
  distribution = qnorm,
  probs = c(0.25, 0.75),
  qtype = 7,
  groups = NULL,
  ci = 0.95,
  alpha = 0.25,
  col = trellis.par.get("plot.line")$col,
  ...,
  col.line
) {
  dots <- list(...)

  if (!missing(col.line)) {
    col <- col.line
  }

  y <- as.numeric(y)
  stopifnot(length(probs) == 2)
  distribution <- get_fun(distribution)

  d <- deparse(formals()[["distribution"]])
  d <- if (grepl("::", d, fixed = TRUE) || grepl(":::", d, fixed = TRUE)) {
    sub("(?<=::)[q]", "d", d, perl = TRUE)
  } else if (grepl("^[q]", d)) {
    sub("^[q]", "d", d)
  } else {
    stop(
      "'distribution' must have a density function counterpart, such as 'qnorm()' ('dnorm()')"
    )
  }

  dens <- eval(parse(text = d))

  nobs <- sum(!is.na(y))
  if (!is.null(groups)) {
    do.call(
      panel.superpose,
      updateList(
        list(
          x = y,
          y = NULL,
          distribution = distribution,
          probs = probs,
          qtype = qtype,
          groups = groups,
          panel.groups = panel.qqmathci,
          ci = ci,
          alpha = alpha
        ),
        dots
      )
    )
  } else if (nobs > 0) {
    n <- length(y)
    pp <- ppoints(y)
    z <- distribution(pp)
    yy <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
    xx <- distribution(probs)
    zz <- distribution(1 - (1 - ci) / 2)
    b <- (yy[2] - yy[1]) / (xx[2] - xx[1])
    a <- yy[1] - b * xx[1]
    se <- b / dens(z) * sqrt(pp * (1 - pp) / n)
    fit <- a + b * z
    upr <- fit + zz * se
    lwr <- fit - zz * se

    do.call(
      panel.polygon,
      updateList(
        list(
          x = c(z, rev(z)),
          y = c(upr, rev(lwr)),
          alpha = alpha,
          col = col,
          border = "transparent"
        ),
        dots
      )
    )
  } else {
    return()
  }
}
