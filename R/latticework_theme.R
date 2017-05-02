#' latticework lattice theme
#'
#' A custom theme for lattice.
#'
#' The theme currently modifies the default lattice theme so that
#'
#' * paddings (margins) are minimized,
#' * axis tick lengths are halved,
#' * fontface for axis labels is set to bold, and
#' * title size is decreased *slightly.*
#'
#' This theme is continually updated and is likely to change. Colors, for
#' instance, are likely to be updated eventually.
#'
#' @inheritParams lattice::standard.theme
#' @param \dots Additional named options appended via [utils::modifyList()].
#'
#' @return A list of graphical parameters that for instance could be supplied
#'   inside a call to [lattice::xyplot()] or set via
#'   [lattice::lattice.options()].
#' @export
#'
#' @examples
#' xyplot(speed ~ dist, data = cars, par.settings = latticework_theme())
#'
latticework_theme <- function(name = "pdf",
                              color = name != "postscript",
                              ...) {
  ll <- lattice::standard.theme(name = name, color = color)

  mod <- list(
    fontsize = list(text = 7, points = 5),
    layout.heights = list(
      top.padding = 0,
      bottom.padding = 0,
      main = 2,
      sub = 2,
      main.key.padding = 0,
      axis.bottom = 1
    ),
    par.main.text = list(cex = 1.1),
    par.xlab.text = list(font = 2),
    par.ylab.text = list(font = 2),
    par.zlab.text = list(font = 2),
    layout.widths = list(right.padding = 0,
                         left.padding = 0,
                         key.right = 1.5),
    axis.components = list(left = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5),
                           right = list(tck = 0.5, pad1 = 0.5, pad2 = 0),
                           bottom = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5),
                           top = list(tck = 0.5, pad1 = 0.5, pad2 = 0))
  )
  utils::modifyList(utils::modifyList(ll, mod), list(...))
}
