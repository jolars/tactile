#' Bubbleplot
#'
#' A lattice-based plot for displaying *bubble plots*, that is, plots that
#' match a variable with the area of circles.
#'
#' @param x A formula of the form `color_variable ~ x * y` followed, optionally,
#'   by `|` and conditioning variables stringed together with `+`.
#' @param bubblekey Draws a bubble key to the right of the plot.
#' @param maxsize Maximum cex for the bubbles.
#' @inheritParams lattice::xyplot
#' @inherit lattice::xyplot return
#' @author Original by Deepayan Sarkar.
#' @return A `trellis` object.
#' @examples
#' bubbleplot(mpg ~ wt * disp, data = mtcars)
#'
#' bubbleplot(mpg ~ wt * disp, data = mtcars, groups = cyl, auto.key = TRUE,
#'            bubblekey = list(title = "Miles/Gallon"))
#' @export
bubbleplot <- function(x, data, ...) UseMethod("bubbleplot")

#' @rdname bubbleplot
#' @export
bubbleplot.formula <-
  function(x,
           data = NULL,
           bubblekey = TRUE,
           maxsize = 2,
           allow.multiple = is.null(groups) || outer,
           outer = !is.null(groups),
           auto.key = FALSE,
           aspect = "fill",
           panel = "panel.bubbleplot",
           prepanel = NULL,
           scales = list(),
           strip = TRUE,
           groups = NULL,
           xlab,
           xlim,
           ylab,
           ylim,
           drop.unused.levels = lattice.getOption("drop.unused.levels"),
           ...,
           lattice.options = NULL,
           default.scales = list(),
           default.prepanel = lattice.getOption("prepanel.default.xyplot"),
           subscripts = !is.null(groups),
           subset = TRUE) {
    formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(x))
    subset <- eval(substitute(subset), data, environment(x))
    if (!is.null(lattice.options)) {
      oopt <- lattice.options(lattice.options)
      on.exit(lattice.options(oopt), add = TRUE)
    }

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    form <-
      latticeParseFormula(
        formula,
        data,
        dimension = 3,
        subset = subset,
        groups = groups,
        multiple = allow.multiple,
        outer = outer,
        subscripts = TRUE,
        drop = drop.unused.levels
      )

    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr
    cond <- form$condition
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    if (length(cond) == 0) {
      strip <- FALSE
      cond <- list(gl(1, length(x)))
    }

    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name

    ## S-PLUS requires both x and y to be numeric, but we
    ## don't. Question is, should we give a warning ? Nope.

    ##if (!(is.numeric(x) && is.numeric(y)))
    ##    warning("x and y are not both numeric")

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <-
      do.call("trellis.skeleton",
              c(list(formula = formula,
                     cond = cond,
                     aspect = aspect,
                     strip = strip,
                     panel = panel,
                     xlab = xlab,
                     ylab = ylab,
                     xlab.default = form$right.x.name,
                     ylab.default = form$right.y.name,
                     lattice.options = lattice.options), dots),
              quote = TRUE)

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent())
    foo$call[[1]] <- quote(xyplot)

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limits)) # override xlim
    {
      have.xlim <- TRUE
      xlim <- foo$x.scales$limits
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limits))
    {
      have.ylim <- TRUE
      ylim <- foo$y.scales$limits
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog)
    {
      xlog <- foo$x.scales$log
      xbase <-
        if (is.logical(xlog)) 10
      else if (is.numeric(xlog)) xlog
      else if (xlog == "e") exp(1)

      x <- log(x, xbase)
      if (have.xlim) xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog)
    {
      ylog <- foo$y.scales$log
      ybase <-
        if (is.logical(ylog)) 10
      else if (is.numeric(ylog)) ylog
      else if (ylog == "e") exp(1)

      y <- log(y, ybase)
      if (have.ylim) ylim <- logLimits(ylim, ybase)
    }

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <- dots
    if (subscripts) foo$panel.args.common$groups <- groups

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length)))
      stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)

    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1) {
      dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
      dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }

    # Compute breakpoints for bubbles etc.
    bubbles <- make_bubbles(z, maxsize)
    z <- bubbles$x

    cond.current.level <- rep(1, length(cond))
    for (packet.number in seq_len(npackets)) {
      id <- compute.packet(cond, cond.current.level)
      foo$packet.sizes[packet.number] <- sum(id)
      foo$panel.args[[packet.number]] <- list(x = x[id], y = y[id], z = z[id])
      if (subscripts)
        foo$panel.args[[packet.number]]$subscripts <- subscr[id]
      cond.current.level <- cupdate(cond.current.level, cond.max.level)
    }

    ## FIXME: make this adjustment everywhere else

    more.comp <-
      c(limits.and.aspect(default.prepanel,
                          prepanel = prepanel,
                          have.xlim = have.xlim, xlim = xlim,
                          have.ylim = have.ylim, ylim = ylim,
                          x.relation = foo$x.scales$relation,
                          y.relation = foo$y.scales$relation,
                          panel.args.common = foo$panel.args.common,
                          panel.args = foo$panel.args,
                          aspect = aspect,
                          npackets = npackets,
                          x.axs = foo$x.scales$axs,
                          y.axs = foo$y.scales$axs),
        cond.orders(foo))
    foo[names(more.comp)] <- more.comp

    if (is.null(foo$legend) && needAutoKey(auto.key, groups))
    {
      foo$legend <-
        list(list(fun = "drawSimpleKey",
                  args =
                    updateList(list(text = levels(as.factor(groups)),
                                    points = TRUE,
                                    rectangles = FALSE,
                                    lines = FALSE),
                               if (is.list(auto.key)) auto.key else list())))
      foo$legend[[1]]$x <- foo$legend[[1]]$args$x
      foo$legend[[1]]$y <- foo$legend[[1]]$args$y
      foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

      names(foo$legend) <-
        if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
          "inside"
      else
        "top"
      if (!is.null(foo$legend[[1]]$args$space))
        names(foo$legend) <- foo$legend[[1]]$args$space
    }

    if (isTRUE(bubblekey) || is.list(bubblekey)) {
      foo$legend <- updateList(foo$legend, list(right = list(
        fun = "draw.key",
        args = list(key = updateList(list(
          title = form$left.name,
          cex.title = 1,
          text = list(as.character(bubbles$breaks)),
          points = list(
            col = if (is.null(groups)) trellis.par.get("plot.symbol")$col else 1,
            pch = trellis.par.get("plot.symbol")$pch,
            fill = trellis.par.get("plot.symbol")$fill,
            alpha = trellis.par.get("plot.symbol")$alpha,
            font = trellis.par.get("plot.symbol")$font,
            cex = bubbles$breaks_cex
          ),
          padding.text = maxsize * 1.3 # Is There any better way to do this?
        ), if (is.list(bubblekey)) bubblekey else list())
      ))))
    }

    class(foo) <- "trellis"
    foo
  }

#' Panel Function for bubbleplot
#'
#' This is a panel function adapted to [bubbleplot()]. Most of the documentation
#' here has been imported from [lattice::xyplot()].
#'
#' @inheritParams lattice::panel.xyplot
#' @param z Variable to map area of the bubbles to.
#'
#' @return A bubble plot panel.
#' @author Original by Deepayan Sarkar. Modifications by Johan Larsson.
#' @examples
#' bubbleplot(wt ~ mpg * disp, groups = cyl, data = mtcars,
#'   panel = function(...) {
#'     panel.grid(...)
#'     panel.bubbleplot(...)
#'     panel.loess(col = 1, ...)
#'   }
#' )
#' @export
panel.bubbleplot <- function(x, y, z, groups = NULL, ...) {
  if (!is.null(groups)) {
    panel.superpose.bubbelplot(x, y, z, groups = groups, ...)
  } else {
    panel.xyplot(x, y, cex = z, ...)
  }
}

panel.superpose.bubbelplot <-
  function(x,
           y = NULL,
           z,
           subscripts,
           groups,
           panel.groups = "panel.xyplot",
           ...,
           col = "black",
           col.line = superpose.line$col,
           col.symbol = superpose.symbol$col,
           pch = superpose.symbol$pch,
           cex = superpose.symbol$cex,
           fill = superpose.symbol$fill,
           font = superpose.symbol$font,
           fontface = superpose.symbol$fontface,
           fontfamily = superpose.symbol$fontfamily,
           lty = superpose.line$lty,
           lwd = superpose.line$lwd,
           alpha = superpose.symbol$alpha,
           type = "p",
           grid = FALSE,
           distribute.type = FALSE) {
  if (distribute.type) {
    type <- as.list(type)
  } else {
    type <- unique(type)
    wg <- match("g", type, nomatch = NA_character_)
    if (!is.na(wg)) {
      if (missing(grid))
        grid <- TRUE
      type <- type[-wg]
    }
    type <- list(type)
  }
  if (grid)
    panel.grid(h = -1, v = -1, x = x, y = y)
  x <- as.numeric(x)
  if (!is.null(y))
    y <- as.numeric(y)
  if (length(x) > 0) {
    if (!missing(col)) {
      if (missing(col.line))
        col.line <- col
      if (missing(col.symbol))
        col.symbol <- col
    }
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    vals <- if (is.factor(groups))
      levels(groups)
    else sort(unique(groups))
    nvals <- length(vals)
    col <- rep(col, length.out = nvals)
    col.line <- rep(col.line, length.out = nvals)
    col.symbol <- rep(col.symbol, length.out = nvals)
    pch <- rep(pch, length.out = nvals)
    fill <- rep(fill, length.out = nvals)
    lty <- rep(lty, length.out = nvals)
    lwd <- rep(lwd, length.out = nvals)
    alpha <- rep(alpha, length.out = nvals)
    font <- rep(font, length.out = nvals)
    if (!is.null(fontface))
      fontface <- rep(fontface, length.out = nvals)
    if (!is.null(fontfamily))
      fontfamily <- rep(fontfamily, length.out = nvals)
    type <- rep(type, length.out = nvals)
    panel.groups <- getFunctionOrName(panel.groups)
    subg <- groups[subscripts]
    ok <- !is.na(subg)
    for (i in seq_along(vals)) {
      id <- ok & (subg == vals[i])
      if (any(id)) {
        args <- list(x = x[id], subscripts = subscripts[id],
                     pch = pch[[i]], cex = z[id], font = font[[i]],
                     fontface = fontface[[i]], fontfamily = fontfamily[[i]],
                     col = col[[i]], col.line = col.line[[i]],
                     col.symbol = col.symbol[[i]],
                     fill = fill[[i]], lty = lty[[i]], lwd = lwd[[i]],
                     alpha = alpha[[i]], type = type[[i]], group.number = i,
                     group.value = vals[i], ...)
        if (!is.null(y))
          args$y <- y[id]
        do.call(panel.groups, args)
      }
    }
  }
}

# Map z values to circle areas and generate pretty breakpoints for bubblekey
# TO DO: Allow 0 to map to arbitrary circle area?
make_bubbles <- function(x, maxsize) {
  stopifnot(all(x >= 0), is.numeric(x))

  # compute required radii for breaks and values
  breaks <- pretty(x)
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
