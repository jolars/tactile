#' Bubbleplots
#'
#' Draws bubbleblots -- trivariate plots where the third dimension is mapped
#' to the size of the points drawn on the screen.
#'
#' @author Johan Larsson
#'
#' @inherit lattice::qqmath return
#' @export
#'
#' @examples
#' bubbleplot(disp ~ hp * wt, groups = cyl, data = mtcars, auto.key = TRUE)
#' bubbleplot(disp ~ hp * mpg | factor(cyl), groups = gear, data = mtcars,
#'            auto.key = TRUE)
bubbleplot <- function(x, data = NULL, ...) UseMethod("bubbleplot")

#' @param x A formula of the form `z ~ x * y`, where `x` and `y` have the usual
#'   interpretation in trellis graphics (see [lattice::xyplot()]) and `z` is
#'   mapped to the size of bubbles.
#' @param data A data.frame, list or environment wherein the formula and
#'   groups arguments can be evaluated.
#' @param maxsize Maximum size (in cex) for the bubbles.
#' @param bubblekey Set to `TRUE` to draw an informative legend about the
#'   bubbles. Uses [lattice::draw.key()]. See the **key** section of the
#'   documentation in [lattice::xyplot()]. If both `auto.key` and `bubblekey`
#'   are given and their `space` arguments (positions) conflict, bubblekey
#'   will silently override the position of `auto.key`.
#' @param panel See [lattice::xyplot()]. Here, we are passing an additional
#'   variable, `z`, which is then used in [panel.bubbleplot()].
#' @param groups See [lattice::xyplot()]
#' @param subset See [lattice::xyplot()]
#' @param outer Ignored.
#' @param allow.multiple Ignored.
#' @param drop.unused.levels See [lattice::xyplot()]
#' @param ... Further arguments to pass to [lattice::xyplot()].
#'
#' @rdname bubbleplot
#' @export
bubbleplot.formula <-
  function(
    x,
    data = NULL,
    maxsize = 3,
    bubblekey = TRUE,
    panel = panel.bubbleplot,
    groups = NULL,
    subset = TRUE,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    ...,
    outer,
    allow.multiple
  ) {
    new_groups <- substitute(groups)
    groups <- eval(substitute(groups), data, environment(x))
    subset <- eval(substitute(subset), data, environment(x))

    if (!missing(outer)) {
      warning("'outer' is ignored.")
    }
    if (!missing(allow.multiple)) {
      warning("'allow.multiple' is ignored.")
    }

    # Parse the formula as z ~ x * y
    form <- latticeParseFormula(
      model = x,
      data = data,
      dimension = 3,
      subset = subset,
      groups = groups,
      multiple = FALSE,
      outer = FALSE,
      subscripts = TRUE,
      drop = drop.unused.levels
    )

    new_form <- xyz_to_xy(form)

    # Compute bubble sizes
    z <- form$left
    bubbles <- make_bubbles(z, maxsize)

    # Retrieve call
    ccall <- match.call()

    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(bubbleplot)

    # Update call
    ccall$x <- new_form
    ccall$z <- bubbles$x
    ccall$panel <- panel

    # Make the call
    ccall[[1]] <- quote(lattice::xyplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall

    # Set up bubblekey (if required)
    ans$legend <- setup_key(
      ans$legend,
      bubblekey,
      list(
        text = list(as.character(bubbles$breaks)),
        title = form$left.name,
        points = list(
          col = if (is.null(groups)) trellis.par.get("plot.symbol")$col else 1,
          pch = trellis.par.get("plot.symbol")$pch,
          fill = trellis.par.get("plot.symbol")$fill,
          alpha = trellis.par.get("plot.symbol")$alpha,
          font = trellis.par.get("plot.symbol")$font,
          cex = bubbles$breaks_cex
        ),
        padding.text = maxsize * 1.3
      ),
      draw.key
    )
    ans
  }

#' Panel Function for Bubble Plots
#'
#' @inheritParams lattice::panel.xyplot
#' @param z A numeric vector that areas of circles will be mapped to.
#' @param groups Grouping variable (see [lattice::xyplot()]).
#' @param cex Is used internally and user settings will be ignored.
#' @param ... Further arguments to pass to [lattice::panel.xyplot()].
#' @param subscripts A vector of indexes to specify which observation to plot.
#'   Normally does not need to be provided by the user.
#'
#' @return Plots a layer inside a panel of a `lattice` plot.
#' @export
panel.bubbleplot <- function(
  x,
  y,
  z,
  groups = NULL,
  subscripts,
  cex = NULL,
  ...
) {
  # include cex as argument in top function and then ignore it

  if (!is.null(groups)) {
    panel.superpose(
      x = x,
      y = y,
      z = z,
      subscripts = subscripts,
      groups = groups,
      panel.bubbleplot,
      ...
    )
  } else {
    panel.xyplot(x, y, cex = z[subscripts], ...)
  }
}

#' Make Bubbles
#'
#' Map `z` to the area of bubbles.
#'
#' @param x A numeric vector.
#' @param maxsize The max size (in cex) of the bubbles.
#'
#' @return A list with the new bubbles as well as pretty breakpoints along with
#'   their respective cex values.
#' @keywords internal
make_bubbles <- function(x, maxsize) {
  stopifnot(all(x >= 0), is.numeric(x))

  # compute required radii for breaks and values
  breaks <- pretty(x, n = 4)

  # Drop first level if it is 0
  if (breaks[1] == 0) {
    breaks <- breaks[-1]
  }

  breaks_cex <- sqrt(breaks / pi)
  x <- sqrt(x / pi)

  # pad with zero and max of breaks and values
  roof <- max(c(x, breaks_cex), na.rm = TRUE)

  breaks_cex <- rescale(c(0, breaks_cex, roof), 0, maxsize)
  breaks_cex <- breaks_cex[-c(1, length(breaks_cex))]

  x <- rescale(c(0, x, roof), 0, maxsize)
  x <- x[-c(1, length(x))]
  list(x = x, breaks = breaks, breaks_cex = breaks_cex)
}
