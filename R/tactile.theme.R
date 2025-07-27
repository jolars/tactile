#' Tactile Theme
#'
#' A custom theme for lattice that tries to make away with some of the
#' (in this author's opinion) excessive margins that result from the default
#' settings. It also provides a different color theme based partly on
#' [latticeExtra::custom.theme()].
#'
#' The theme currently modifies the default lattice theme so that
#'
#' * paddings (margins) are minimized,
#' * axis tick lengths are halved, and
#' * title size is decreased *slightly*.
#'
#' @param fontsize A vector of two numeric scalars for text and symbols
#'   respectively.
#' @param ... Additional named options.
#' @param color Colorized theme.
#'
#' @return A list of graphical parameters that for instance could be supplied
#'   inside a call to [lattice::xyplot()] or set via
#'   [lattice::lattice.options()].
#'
#' @export
#'
#' @examples
#' xyplot(speed ~ dist, data = cars, par.settings = tactile.theme())
#' opars <- trellis.par.get()
#' trellis.par.set(tactile.theme())
#' show.settings()
#' trellis.par.set(opars)
tactile.theme <- function(fontsize = c(12, 8), color = TRUE, ...) {
  theme <- standard.theme(name = "pdf", color = color)

  theme <- updateList(
    theme,
    list(
      fontsize = list(text = fontsize[1], points = fontsize[2]),
      strip.background = list(col = rep("grey95", 7)),
      strip.shingle = list(col = rep("grey70", 7)),
      strip.text = list(cex = 0.75),
      layout.heights = list(
        top.padding = 0.1,
        bottom.padding = 0.1,
        axis.top = 0.75,
        main = 2,
        main.key.padding = 0,
        sub = 2,
        xlab = 1.25,
        key.top = 1.25
      ),
      layout.widths = list(
        right.padding = 0.1,
        left.padding = 0.5,
        key.right = 1.25,
        ylab = 1.25
      ),
      par.main.text = list(cex = 1.15),
      axis.components = list(
        left = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5),
        right = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5),
        bottom = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5),
        top = list(tck = 0.5, pad1 = 0.5, pad2 = 0.5)
      )
    )
  )

  if (color) {
    symbol <- RColorBrewer::brewer.pal(n = 9, "Set1")[c(2:1, 3:5, 7:9)]
    fill <- adjustcolor(
      symbol,
      offset = c(0.5, 0.5, 0.5, 0),
      transform = diag(c(0.6, 0.6, 0.6, 0.6))
    )

    region <- RColorBrewer::brewer.pal(n = 11, "Spectral")
    reference <- "#e8e8e8"
    bg <- "transparent"
    fg <- "black"

    theme <- updateList(
      theme,
      list(
        add.line = list(col = fg),
        add.text = list(col = fg),
        axis.line = list(col = fg),
        axis.text = list(col = fg),
        background = list(col = bg),
        box.rectangle = list(col = fg, fill = fill[1]),
        box.dot = list(pch = "|", col = fg),
        box.umbrella = list(col = fg, lty = 1),
        box.3d = list(col = fg),
        dot.line = list(col = reference),
        dot.symbol = list(col = symbol[1]),
        par.xlab.text = list(col = fg),
        par.ylab.text = list(col = fg),
        par.zlab.text = list(col = fg),
        par.main.text = list(col = fg),
        par.sub.text = list(col = fg),
        plot.line = list(col = symbol[1]),
        plot.polygon = list(col = fill[1], border = fg),
        plot.symbol = list(col = symbol[1]),
        reference.line = list(col = reference),
        regions = list(col = grDevices::colorRampPalette(region)(100)),
        superpose.line = list(col = symbol),
        superpose.polygon = list(col = fill),
        superpose.symbol = list(col = symbol, fill = fill),
        strip.border = list(col = fg)
      )
    )
  }
  updateList(theme, list(...))
}
