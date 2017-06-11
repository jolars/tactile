#' Q-Q Diagram for Zoo Time Series
#'
#' Draw quantile-Quantile plots of a sample against a theoretical distribution,
#' possibly conditioned on other variables.
#'
#' @param x A `zoo` object
#' @param data Ignored
#' @param ref Plots a reference line via [lattice::panel.qqmathline]
#' @param ci Plots confidence levels via [panel.qqmathci]
#' @param \dots Parameters to pass on to [lattice::qqmath()].
#'
#' @return Plots and returns a `trellis` object.
#' @author Original by Deepayan Sarkar. Method by Johan Larsson.
#' @seealso [lattice::qqmath()], [zoo::zoo()], [lattice::panel.qqmathline()].
#' @export
#'
#' @examples
#' qqmath(zoo::zoo(lh))
qqmath.zoo <- function(x,
                       data = NULL,
                       xlab = "Theoretical quantiles",
                       ylab = "Sample quantiles",
                       ref = TRUE,
                       ci = TRUE,
                       ...) {
  do.call(qqmath, updateList(list(
    x = ~ zoo::coredata(x),
    xlab = xlab,
    ylab = ylab,
    panel = function(...) {
      if (ci)
        panel.qqmathci(...)
      if (ref)
        panel.qqmathline(...)
      panel.qqmath(...)
    }
  ), list(...)))
}
