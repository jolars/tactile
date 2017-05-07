#' Panel function for Q-Q plot confidence intervals
#'
#' Panel function to go along with [lattice::qqmath()] and possibly
#' [lattice::panel.qqmathline()]. Adds filled confidence bands to the Q-Q-plot.
#'
#' The function tries to figure out the density function counterpart to
#' that provided in the argument `distribution`.
#'
#' @inheritParams lattice::panel.qqmathline
#' @param ci Confidence level
#' @param ci_col Fill color of the confidence bands
#' @param \dots Arguments passed to [lattice::panel.superpose()] and
#'   [lattice::panel.polygon()].
#'
#' @author Modified by Johan Larsson from [lattice::panel.qqmathline()] by
#' Deepayan Sarkar.
#' @seealso [lattice::panel.qqmathline()], [lattice::qqmath()], and
#'   [lattice::panel.qqmath()].
#'
#' @return Adds confidence levels to a trellis device such as that
#'   created by [lattice::qqmath()].
#' @export
#'
#' @examples
#' library(lattice)
#' qqmath(~ Sepal.Width | Species, data = iris, distribution = stats::qnorm,
#'        panel = function(x, ...) {
#'          panel.qqmathci(x, ...)
#'          panel.qqmathline(x, ...)
#'          panel.qqmath(x, ...)
#'        })
panel.qqmathci <- function(x,
                           y = x,
                           distribution = stats::qnorm,
                           probs = c(0.25, 0.75),
                           qtype = 7,
                           groups = NULL,
                           ci = 0.95,
                           ci_col = "grey90",
                           ...,
                           identifier = "qqmathci") {
  y <- as.numeric(y)
  stopifnot(length(probs) == 2)
  distribution <- get_fun(distribution)

  d <- deparse(formals()[["distribution"]])
  d <- if (grepl("::", d, fixed = TRUE) || grepl(":::", d, fixed = TRUE))
    sub("(?<=::)[q]", "d", d, perl = TRUE)
  else if (grepl("^[q]", d))
    sub("^[q]", "d", d)
  else
    stop("'distribution' must have a density function counterpart, such as 'qnorm()' ('dnorm()')")

  dens <- eval(parse(text = d))

  nobs <- sum(!is.na(y))
  if (!is.null(groups))
    lattice::panel.superpose(
      x = y,
      y = NULL,
      distribution = distribution,
      probs = probs,
      qtype = qtype,
      groups = groups,
      panel.groups = panel.qqmathci,
      ...
    )
  else if (nobs > 0) {
    n   <- length(y)
    pp  <- stats::ppoints(y)
    z   <- distribution(pp)
    yy  <- stats::quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
    xx  <- distribution(probs)
    zz  <- distribution(1 - (1 - ci)/2)
    b   <- (yy[2] - yy[1]) / (xx[2] - xx[1])
    a   <- yy[1] - b * xx[1]
    se  <- (b/dens(z)) * sqrt(pp * (1 - pp)/n)
    fit <- a + b * z
    upr <- fit + zz * se
    lwr <- fit - zz * se

    lattice::panel.polygon(x = c(z, rev(z)),
                           y = c(upr, rev(lwr)),
                           col = ci_col,
                           border = "transparent",
                           ...,
                           identifier = identifier)
  }
}
