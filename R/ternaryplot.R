#' Ternary Plot
#'
#' A ternary plot is a triangular diagram that displays proportions of
#' three variables. It can be used to map three-dimensional data to a
#' two-dimensional surface with the caveat that the data's original scales are
#' lost (unless it was proportional data to begin with).#'
#'
#' @inherit lattice::levelplot return
#' @param x See **Methods (by class)**.
#' @param data A data frame in which the `formula`, `groups`, and conditioning
#'   variables are evaluated.
#'
#' @export
#'
#' @examples
#' ternaryplot(Fertility ~ Agriculture * Catholic, data = swiss)
#' ternaryplot(Catholic ~ Examination * Education, response = Infant.Mortality,
#'             data = swiss, contour = FALSE)
#'
#' ternaryplot(Or ~ An * Ab | Feldspar, data = feldspar)
#'
#' ternaryplot(Or ~ An * Ab, groups = Feldspar, data = feldspar, density = TRUE)
ternaryplot <- function(x, data, ...) {
  UseMethod("ternaryplot")
}

#' @inheritParams panel.ternaryplot
#' @param groups A variable or expression to be evaluated in `data` and used to
#'   distinguish groups by varying graphical parameters.
#' @param colorkey if `TRUE` automatically computes a colorkey for density
#'   or response estimates. Can also be a list (see [lattice::levelplot()] for
#'   details on this).
#' @param xlim X limits for the plot region.
#' @param ylim Y limits for the plot region.
#' @param panel The panel function.
#' @param default.prepanel The default prepanel function.
#' @param drop.unused.levels Drop unused conditioning or groups levels.
#' @param subset An expression that evaluates to a logical or integer indexing
#'   vector. Like groups, it is evaluated in data. Only the resulting rows of
#'   data are used for the plot.
#' @param ... Arguments that are passed on to other methods, particularly
#'   [panel.ternaryplot()].
#'
#' @describeIn ternaryplot A formula of the form `top ~ left * right`. Variables
#'   will be evaluated inside data if provided.
#' @export
ternaryplot.formula <-
  function(
    x,
    data = NULL,
    response = NULL,
    groups = NULL,
    density = FALSE,
    region = density || !is.null(response),
    contour = density || !is.null(response),
    labels = !is.null(response),
    colorkey = region,
    xlab,
    ylab,
    zlab,
    xlim = c(-0.15, 1.15),
    ylim = c(-0.30, 1),
    panel = panel.ternaryplot,
    default.prepanel = lattice.getOption("prepanel.default.xyplot"),
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    subset = TRUE,
    ...
  ) {
    response <- eval(substitute(response), data, environment(x))
    groups <- eval(substitute(groups), data, environment(x))
    subset <- eval(substitute(subset), data, environment(x))
    form <- latticeParseFormula(
      x,
      data,
      dimension = 3,
      subset = subset,
      groups = groups,
      multiple = FALSE,
      outer = FALSE,
      subscripts = TRUE,
      drop = drop.unused.levels
    )

    # Retrieve call
    ccall <- match.call()
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(ternaryplot)

    # Update call
    ccall$response <- response
    ccall$panel <- panel
    ccall$default.prepanel <- default.prepanel
    ccall$density <- density
    ccall$contour <- contour
    ccall$region <- region
    ccall$data <- data
    ccall$aspect <- "iso"
    ccall$xlim <- xlim
    ccall$ylim <- ylim

    if (density && !is.null(form$condition)) {
      iso <- with(form, ilr(left, right.x, right.y))
      dens <- by(iso, form$condition, function(x) {
        MASS::kde2d(x$x, x$y, n = 50, lims = c(-16.92, 16.92, -14.65, 14.65))$z
      })
      dens <- do.call(rbind, dens)
      ccall$density_breaks <- pretty(dens, 10)
    } else if (density) {
      iso <- with(form, ilr(left, right.x, right.y))
      dens <- MASS::kde2d(
        iso$x,
        iso$y,
        n = 50,
        lims = c(-16.92, 16.92, -14.65, 14.65)
      )$z
      ccall$density_breaks <- pretty(dens, 10)
    }

    # Make the call
    ccall[[1]] <- quote(lattice::cloud)
    ans <- eval.parent(ccall)
    ans$call <- ocall

    # Set up colorkey (if needed)
    ans$legend <- setup_key(
      ans$legend,
      colorkey,
      if (density) {
        list(col = seq_pal(100), at = ccall$density_breaks)
      } else if (!is.null(response) && region) {
        list(
          col = trellis.par.get("regions")$col,
          at = pretty(quantile(response, c(0.1, 0.9)), 10)
        )
      } else {
        list()
      },
      draw.colorkey
    )

    ans$x.limits <- xlim
    ans$y.limits <- ylim

    ans
  }

#' @describeIn ternaryplot A data frame for which the first three columns will
#'   be mapped to the *left*, *right*, and *top* dimensions of the ternary plot
#'   respectively.
#' @export
ternaryplot.data.frame <- function(x, data = NULL, ...) {
  if (NCOL(x) < 3) {
    stop("'x' needs at least 3 columns.")
  }
  if (!is.null(data)) {
    warning("Exlicit 'data' specification has been dropped.")
  }

  names <- colnames(x)
  form <- as.formula(paste(names[3], "~", names[1], "*", names[2]))
  ternaryplot(form, data = x, ...)
}

#' @describeIn ternaryplot A matrix for which the
#'   first three columns will be mapped to the *left*, *right*, and *top*
#'   dimensions of the ternary plot respectively.
#' @export
ternaryplot.matrix <- function(x, data = NULL, ...) {
  ternaryplot(as.data.frame(x), data = data, ...)
}

#' Panel Function for Ternary Plots
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param z Numeric vector
#' @param grid Draw a reference grid.
#' @param response An optional response variable
#' @param density Compute two-dimensional density estimates via [MASS::kde2d()].
#' @param region Fill density or response estimates with a color gradient.
#' @param contour Draw contour lines for density and response estimates.
#' @param labels Label contour lines.
#' @param xlab X axis label (the left dimension)
#' @param ylab Y axis label (the right dimension)
#' @param zlab Z axis label (the top dimension)
#' @param xlab.default Internal argument
#' @param ylab.default Internal argument
#' @param zlab.default Internal argument
#' @param ... Arguments passed down to subsequent panel functions.
#' @param subscripts See [lattice::panel.xyplot()].
#' @param points Draw points (via [panel.ternaryplot.xyplot()]).
#' @param density_breaks Breaks for the density plot if used (see
#'   [panel.ternaryplot.density()]).
#'
#' @seealso The building blocks of this function are available as the separate
#'   panel functions [panel.ternaryplot.xyplot()], [panel.ternaryplot.grid()],
#'   [panel.ternaryplot.scales()], [panel.ternaryplot.clip()],
#'   [panel.ternaryplot.response()], and [panel.ternaryplot.density()] in case
#'   the user would like to get complete control of the customization.
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot <- function(
  x,
  y,
  z,
  subscripts,
  response = NULL,
  density = FALSE,
  region = density || !is.null(response),
  contour = density || !is.null(response),
  labels = !is.null(response),
  points = TRUE,
  grid = TRUE,
  density_breaks = NULL,
  xlab,
  ylab,
  zlab,
  xlab.default,
  ylab.default,
  zlab.default,
  ...
) {
  if (length(subscripts) == 0) {
    return()
  }

  xyz <- data.frame(x, y, z)[subscripts, ]
  lrt <- xyz / rowSums(xyz)
  xy <- tern_cart(lrt)

  if (grid) {
    panel.ternaryplot.grid()
  }

  # Density?
  if (density || contour) {
    panel.ternaryplot.density(
      x = x,
      y = y,
      z = z,
      subscripts = subscripts,
      region = region,
      contour = contour,
      labels = labels,
      density_breaks = density_breaks,
      ...
    )
  } else if (!is.null(response)) {
    panel.ternaryplot.response(
      x = x,
      y = y,
      z = z,
      subscripts = subscripts,
      response = response,
      region = region,
      contour = contour,
      labels = labels,
      ...
    )
  }

  # Clip
  panel.ternaryplot.clip()

  # Draw ticks
  panel.ternaryplot.scales(xlab, ylab, zlab, at = seq.int(0, 1, by = 0.2))

  # Overlay points
  if (points) {
    panel.ternaryplot.xyplot(x = x, y = y, z = z, subscripts = subscripts, ...)
  }
}

#' Axes and Labels for Ternary Plots
#'
#' @inheritParams lattice::panel.cloud
#' @param at Where to draw tick marks.
#' @param ... Currently ignored.
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.scales <- function(
  xlab,
  ylab,
  zlab,
  xlab.default,
  ylab.default,
  zlab.default,
  at = seq.int(0, 1, by = 0.2),
  ...
) {
  axis.components <- trellis.par.get("axis.components")
  axis.padding <- lattice.getOption("axis.padding")
  axis.left <- lattice.getOption("axis.left")
  axis.text <- trellis.par.get("axis.text")
  axis.line <- trellis.par.get("axis.line")
  if (axis.line$col == "transparent") {
    axis.line$col <- 1
  } # Don't allow axis.line$col to remove axis
  axis.units <- lattice.getOption("axis.units")
  layout.widths <- trellis.par.get("layout.widths")
  ylab.axis.padding <- lattice.getOption("layout.widths")$ylab.axis.padding
  xlab.text <- trellis.par.get("par.xlab.text")
  ylab.text <- trellis.par.get("par.ylab.text")
  zlab.text <- trellis.par.get("par.zlab.text")

  gpar.axis.text <- do.call(gpar, axis.text)
  gpar.axis.line <- do.call(gpar, axis.line)

  if (is.null(xlab) && !is.null(xlab.default)) {
    xlab <- xlab.default
  }
  if (is.null(ylab) && !is.null(ylab.default)) {
    ylab <- ylab.default
  }
  if (is.null(zlab) && !is.null(zlab.default)) {
    zlab <- zlab.default
  }

  tck <- axis.units$outer$left$tick
  pad1 <- axis.units$outer$left$pad1
  pad2 <- axis.units$outer$left$pad2

  tck$x <- tck$x * axis.components$left$tck
  pad1$x <- pad1$x * axis.components$left$pad1
  pad2$x <- pad2$x * axis.components$left$pad2

  rot <- 60L * pi / 180L

  tck_x1 <- unit(cos(rot) * tck$x, tck$units)
  tck_x2 <- unit(cos(rot / 2L) * tck$x, tck$units)
  tck_y1 <- unit(sin(rot) * tck$x, tck$units)
  tck_y2 <- unit(sin(rot / 2L) * tck$x, tck$units)

  pad1_x1 <- unit(cos(rot) * pad1$x, pad1$units)
  pad1_x2 <- unit(cos(rot / 2L) * pad1$x, pad1$units)
  pad1_y1 <- unit(sin(rot) * pad1$x, pad1$units)
  pad1_y2 <- unit(sin(rot / 2L) * pad1$x, pad1$units)

  pad2_x1 <- unit(cos(rot) * pad2$x, pad2$units)
  pad2_x2 <- unit(cos(rot / 2L) * pad2$x, pad2$units)
  pad2_y1 <- unit(sin(rot) * pad2$x, pad2$units)
  pad2_y2 <- unit(sin(rot / 2L) * pad2$x, pad2$units)

  tck <- unit(tck$x, tck$units)
  pad1 <- unit(pad1$x, pad1$units)
  pad2 <- unit(pad2$x, pad2$units)

  top <- sqrt(3L) / 2L

  bx <- unit(at, "native")
  by <- unit(rep.int(0, length(at)), "native")
  lx <- unit(cos(rot) * at, "native")
  ly <- unit(sin(rot) * at, "native")
  rx <- unit(cos(rot) * at + 0.5, "native")
  ry <- unit(rev(sin(rot) * at), "native")

  # Draw axis ticks
  grid.segments(
    x0 = unit.c(bx, lx, rx),
    y0 = unit.c(by, ly, ry),
    x1 = unit.c(bx - tck_x1, lx - tck_x1, rx + tck),
    y1 = unit.c(by - tck_y1, ly + tck_y1, ry),
    default.units = "native",
    gp = gpar.axis.line
  )

  # Draw bounding triangle
  grid.lines(
    x = c(0L, 1L, 0.5, 0L),
    y = c(0L, 0L, top, 0L),
    default.units = "native",
    gp = gpar.axis.line
  )

  # Draw axis units on the left
  grid.text(
    label = rev(at),
    x = lx - tck_x1 - pad1_x1,
    y = ly + tck_y1 + pad1_y1,
    rot = -rot * 180L / pi,
    just = "right",
    gp = gpar.axis.text
  )

  # Draw axis units on the right
  grid.text(
    label = rev(at),
    x = rx + tck + pad1,
    y = ry,
    just = "left",
    gp = gpar.axis.text
  )

  # Draw axis units on the bottom
  grid.text(
    label = at,
    x = bx - tck_x1 - pad1_x1,
    y = by - tck_y1 - pad1_y1,
    rot = rot * 180L / pi,
    just = "right",
    gp = gpar.axis.text
  )

  # Draw axis labels
  nudge <- max(stringWidth(at))
  nudgex <- nudge * cos(rot / 2L)
  nudgey <- nudge * sin(rot / 2L)

  grid.text(
    label = xlab,
    x = unit(0.25, "native") - tck_x2 - pad1_x2 - pad2_x2 - nudgex,
    y = unit(top / 2L, "native") + tck_y2 + pad1_y2 + pad2_y2 + nudgey,
    rot = rot * 180L / pi,
    just = "bottom",
    gp = do.call(gpar, xlab.text)
  )

  grid.text(
    label = ylab,
    x = unit(0.5, "native"),
    y = unit(0, "native") - tck - pad1 - pad2 - nudge,
    just = "top",
    gp = do.call(gpar, ylab.text)
  )

  grid.text(
    label = zlab,
    x = unit(0.75, "native") + tck_x2 + pad1_x2 + pad2_x2 + nudgex,
    y = unit(top / 2L, "native") + tck_y2 + pad1_y2 + pad2_y2 + nudgey,
    rot = -rot * 180L / pi,
    just = "bottom",
    gp = do.call(gpar, zlab.text)
  )
}

#' Reference Grid for Ternary Plot
#'
#' @param at Where to draw the reference lines
#' @param alpha Alpha
#' @param col Color
#' @param lty Line type
#' @param lwd Line weight
#'
#' @export
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.grid <- function(
  at = seq.int(0, 1, by = 0.2),
  alpha = reference.line$alpha,
  col = reference.line$col,
  lty = reference.line$lty,
  lwd = reference.line$lwd
) {
  reference.line <- trellis.par.get("reference.line")

  top <- sqrt(3L) / 2L

  rot <- 60L * pi / 180L

  bx <- at
  by <- rep.int(0, length(at))
  lx <- cos(rot) * at
  ly <- sin(rot) * at
  rx <- lx + 0.5
  ry <- rev(ly)

  grid.segments(
    x0 = c(lx, rx, lx),
    y0 = c(ly, ry, ly),
    x1 = c(bx, bx, rev(rx)),
    y1 = c(by, by, rev(ry)),
    default.units = "native",
    gp = gpar(alpha = alpha, col = col, lty = lty, lwd = lwd)
  )
}

#' Response Panels for Ternary Plots
#'
#' @inheritParams panel.ternaryplot
#' @param fun Function to apply to the response variable.
#' @param formula Formula for the function.
#' @param ... Arguments passed on to [lattice::panel.lines()],
#'   [lattice::panel.polygon()], [lattice::panel.text()].
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.response <- function(
  x,
  y,
  z,
  subscripts,
  response,
  region = TRUE,
  contour = TRUE,
  labels = isTRUE(contour),
  fun = c("glm", "lm"),
  formula = response ~ poly(x, y),
  ...
) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(z))
  dots <- list(...)

  if (missing(response)) {
    stop("`panel.ternaryplot.response` needs a response variable.")
  }

  add.line <- trellis.par.get("add.line")
  add.text <- trellis.par.get("add.text")
  regions <- trellis.par.get("regions")

  iso <- ilr(x, y, z)[subscripts, ]
  iso$response <- response[subscripts]

  xseq <- exp(-seq(0, 6, length.out = 50)) * 16L
  xseq <- c(-xseq, rev(xseq))
  yseq <- exp(-seq(0, 6, length.out = 50)) * 14L
  yseq <- c(-yseq, rev(yseq))

  grid <- expand.grid(x = xseq, y = yseq)

  grid$pred <- switch(
    match.arg(fun),
    glm = predict(glm(formula, data = iso), newdata = grid),
    lm = predict(lm(formula, data = iso), newdata = grid)
  )

  grid <- na.omit(grid)

  z <- tapply(grid$pred, grid[c("x", "y")], identity)

  clines <- grDevices::contourLines(
    x = sort.int(unique(grid$x)),
    y = sort.int(unique(grid$y)),
    z = z,
    levels = pretty(quantile(response, c(0.1, 0.9)), 10L)
  )

  clines <- lapply(clines, function(x) {
    list(level = x$level, xy = tern_cart(ilr_inv(x$x, x$y)))
  })

  # Regions fills
  if (region) {
    levels <- vapply(clines, "[[", "level", FUN.VALUE = numeric(1))
    numcol <- length(levels) - 1L
    fills <- level.colors(
      x = seq_len(numcol) - 0.5,
      at = seq_len(numcol + 1L) - 1L,
      col.regions = regions$col
    )

    for (i in seq_along(clines)[-length(clines)]) {
      a <- clines[[i + 1]]$xy
      b <- clines[[i]]$xy
      polys <- rbind(a, b[rev(seq_along(b$x)), ])

      do.call(
        panel.polygon,
        updateList(
          list(
            polys,
            border = "transparent",
            col = fills[i],
            lty = add.line$lty,
            lwd = add.line$lwd
          ),
          dots
        )
      )
    }
  }

  # Draw contour lines
  if (contour) {
    for (i in seq_along(clines)) {
      foo <- clines[[i]]
      xy <- foo$xy

      if (labels && NROW(xy) > 100L) {
        ad <- vapply(xy, diff, FUN.VALUE = double(nrow(xy) - 1L))
        as <- sqrt(rowSums(ad^2L))
        mid <- which.min(abs(cumsum(as) - sum(as) / 2L))

        xx <- xy$x[c(mid - 1L, mid + 1L)]
        xx <- xy$y[c(mid - 1L, mid + 1L)]

        dy <- diff(xy$y[c(mid + 1L, mid - 1L)])
        dx <- diff(xy$x[c(mid + 1L, mid - 1L)])
        angle <- atan2(dy, dx) * 180L / pi

        if (abs(angle) > 90L) {
          angle <- angle - 180L
        }

        do.call(
          panel.text,
          updateList(
            list(
              x = xy$x[mid],
              y = xy$y[mid],
              adj = c(0.5, 0L),
              labels = foo$level,
              srt = angle
            ),
            dots
          )
        )
      }
      do.call(
        panel.lines,
        updateList(
          list(
            xy,
            border = "transparent",
            col = add.line$col,
            lty = add.line$lty,
            lwd = add.line$lwd
          ),
          dots
        )
      )
    }
  }
}

#' Two-Dimensional Density Estimation for Ternary Plots
#'
#' @inheritParams panel.ternaryplot
#' @inheritParams MASS::kde2d
#' @param ... Arguments that will be passed on to [lattice::panel.lines()],
#'   [lattice::panel.polygon()], and [lattice::panel.text()].
#'
#' @inherit panel.ternaryplot return
#' @export
panel.ternaryplot.density <- function(
  x,
  y,
  z,
  subscripts,
  n = 100,
  region = TRUE,
  contour = FALSE,
  labels = isTRUE(contour),
  density_breaks = NULL,
  ...
) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(z))

  regions <- trellis.par.get("regions")
  add.line <- trellis.par.get("add.line")

  iso <- ilr(x, y, z)[subscripts, ]
  dens <- MASS::kde2d(
    iso$x,
    iso$y,
    n = n,
    lims = c(-16.92, 16.92, -14.65, 14.65)
  )
  if (is.null(density_breaks)) {
    density_breaks <- pretty(dens$z, 10)
  }

  clines <- with(
    dens,
    grDevices::contourLines(x, y, z, levels = density_breaks)
  )

  clines <- lapply(clines, function(x) {
    list(level = x$level, xy = tern_cart(ilr_inv(x$x, x$y)))
  })

  if (region) {
    levels <- vapply(clines, "[[", "level", FUN.VALUE = numeric(1))
    numcol <- length(levels) - 1L
    fills <- level.colors(
      x = seq_len(numcol) - 0.5,
      at = seq_len(numcol + 1L) - 1L,
      col.regions = seq_pal(100L)
    )

    for (i in seq_along(clines)) {
      polys <- clines[[i]]$xy
      do.call(
        panel.polygon,
        updateList(
          list(
            polys,
            border = "transparent",
            col = fills[i],
            alpha = regions$alpha
          ),
          list(...)
        )
      )
    }
  }

  if (contour) {
    for (i in seq_along(clines)) {
      foo <- clines[[i]]
      xy <- foo$xy

      if (labels && NROW(xy) > 5L) {
        ad <- vapply(xy, diff, FUN.VALUE = double(nrow(xy) - 1L))
        as <- sqrt(rowSums(ad^2L))
        mid <- which.min(abs(cumsum(as) - sum(as) / 2L))

        xx <- xy$x[c(mid - 1L, mid + 1L)]
        xx <- xy$y[c(mid - 1L, mid + 1L)]

        dy <- diff(xy$y[c(mid + 1L, mid - 1L)])
        dx <- diff(xy$x[c(mid + 1L, mid - 1L)])
        angle <- atan2(dy, dx) * 180L / pi

        if (abs(angle) > 90L) {
          angle <- angle - 180L
        }

        do.call(
          panel.text,
          updateList(
            list(
              x = xy$x[mid],
              y = xy$y[mid],
              adj = c(0.5, 0L),
              labels = foo$level,
              srt = angle
            ),
            list(...)
          )
        )
      }
      do.call(
        panel.lines,
        updateList(
          list(
            xy,
            border = "transparent",
            col = add.line$col,
            lty = add.line$lty,
            lwd = add.line$lwd
          ),
          list(...)
        )
      )
    }
  }
}

#' Ternary Plot Wrapper for lattice::xyplot
#'
#' This mainly exists to enable users to string together their own ternary plot
#' functions.
#'
#' @param x Numeric vector of values in the original space
#' @param y Numeric vector of values in the original space
#' @param z Numeric vector of values in the original space
#' @param subscripts see [lattice::xyplot()].
#' @param ... Arguments that are passed on to [lattice::panel.xyplot()].
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.xyplot <- function(x, y, z, subscripts, ...) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(z))

  xyz <- data.frame(x, y, z)[subscripts, ]
  lrt <- xyz / rowSums(xyz)
  xy <- tern_cart(lrt)

  panel.xyplot(xy$x, xy$y, subscripts = subscripts, ...)
}

#' Plot Region Clipping for Ternary Plots
#'
#' @param xl X axis limits
#' @param yl Y axis limits
#' @param border Border color
#' @param col Polygon fill
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.clip <- function(
  xl = current.panel.limits()$x,
  yl = current.panel.limits()$y,
  border = "transparent",
  col = if (background$col == "transparent") "#FFFFFF" else background$col
) {
  background <- trellis.par.get("background")

  polx <- c(xl[1], 0, 0.5, 1, xl[1], xl[1], xl[2], xl[2], xl[1], xl[1])
  poly <- c(0, 0, sqrt(3) / 2, 0, 0, yl[1], yl[1], yl[2], yl[2], 0)
  panel.polygon(polx, poly, col = col, border = border)
}

# Convert from ternary to cartesian coordinates
tern_cart <- function(l, r = NULL, t = NULL) {
  if (NCOL(l) > 1L) {
    r <- l[, 2L]
    t <- l[, 3L]
    l <- l[, 1L]
  }
  x <- 0.5 * (2L * r + t) / (l + r + t)
  y <- sqrt(3L) / 2L * t / (l + r + t)
  data.frame(x, y)
}

# Convert from cartesian to ternary coordinates
cart_tern <- function(x, y = NULL) {
  if (NCOL(x) > 1L) {
    y <- x[, 2L]
    x <- x[, 1L]
  }
  r <- x - y * tan(pi / 6L)
  t <- y / (tan(pi / 3L) * 0.5)
  l <- 1L - r - t
  data.frame(l, r, t)
}

# Isometric log-ratio transformation
ilr <- function(x, y = NULL, z = NULL) {
  if (any(x < 0L)) {
    stop("No negative values, please.")
  }

  if (!is.null(y) || !is.null(z)) {
    x <- data.frame(x, y, z)
  }

  data.frame(
    x = -sqrt(2L / 3L) *
      log(exp(rowSums(log(as.matrix(x[, c(2L, 3L)]))) / 2L) / x[, 1L]),
    y = -sqrt(0.5) * log(x[, 3L] / x[, 2L])
  )
}

# Inverse isometric log-ratio transformation
ilr_inv <- function(x, y = NULL) {
  if (!is.null(y)) {
    x <- data.frame(x, y)
  }

  x <- -x
  n <- nrow(x)
  xyz <- data.frame(x = double(n), y = double(n), z = double(n))

  xyz[, 1L] <- -sqrt(2 / 3) * x[, 1L]
  xyz[, 2L] <- x[, 1L] / sqrt(6)
  xyz[, 3L] <- xyz[, 2L] + x[, 2L] / sqrt(2L)
  xyz[, 2L] <- xyz[, 2L] - x[, 2L] * sqrt(0.5)

  exp(xyz) / rowSums(exp(as.matrix(xyz)))
}
