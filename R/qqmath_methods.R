#' Q-Q diagram for zoo time series
#'
#' Draw quantile-Quantile plots of a sample against a theoretical distribution,
#' possibly conditioned on other variables.
#'
#' @param x A `zoo` object
#' @param data Ignored
#' @param reference Plot a reference line via [lattice::panel.qqmathline]?
#' @param ci Plot confidence levels?
#' @param \dots Graphical parameters to pass on to [lattice::qqmath()].
#'
#' @return Plots and returns a `trellis` object.
#' @seealso [lattice::qqmath()], [zoo::zoo()], [lattice::panel.qqmathline()].
#' @export
#'
#' @examples
#' library(zoo)
#' zoo_lh <- zoo(lh)
#' qqmath(zoo_lh)
qqmath.zoo <- function(x, data = NULL, reference = TRUE, ci = TRUE, ...) {
  ll <- list(
    x = ~ zoo::coredata(x),
    xlab = "Theoretical quantiles",
    ylab = "Sample quantiles",
    panel = function(x, y, ...) {
      if (ci)
        panel.qqmathci(x, ...)
      if (reference)
        lattice::panel.qqmathline(x, col = "gray50", lty = 2)
      lattice::panel.qqmath(x, ...)
    }
  )
  ll <- update_list(ll, list(...))
  do.call(lattice::qqmath, ll)
}
