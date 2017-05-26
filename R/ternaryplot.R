#' Ternary Plot
#'
#' A ternary diagram is a triangular diagram that displays proportions of
#' three variables.
#'
#' @param x
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' ternaryplot(Fertility ~ Agriculture * Catholic, data = swiss)
ternaryplot <- function(x, data = NULL, ...) UseMethod("ternaryplot")

#' Title
#'
#' @param x
#' @param data
#' @param allow.multiple
#' @param outer
#' @param panel
#' @param groups
#' @param subset
#' @param drop.unused.levels
#' @param ...
#'
#' @return
#' @export
ternaryplot.formula <- function(
  x,
  data = NULL,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  allow.multiple = is.null(groups) || outer,
  outer = !is.null(groups),
  panel = panel.ternaryplot,
  groups = NULL,
  subset = TRUE,
  drop.unused.levels = lattice.getOption("drop.unused.levels"),
  ...
) {
  new_groups <- substitute(groups)
  groups <- eval(substitute(groups), data, environment(x))
  subset <- eval(substitute(subset), data, environment(x))

  # Parse the formula as z ~ x * y
  form <- latticeParseFormula(
    model = x,
    data = data,
    dimension = 3,
    subset = subset,
    groups = groups,
    multiple = allow.multiple,
    outer = outer,
    subscripts = TRUE,
    drop = drop.unused.levels
  )

  x <- form$right.x
  y <- form$right.y
  z <- form$left

  # Compute and plot relative fractions (new points)
  xx <- 0.5 * (2 * y + z) / (x + y + z)
  yy <- top * z / (x + y + z)

  if (is.null(data))
    data <- data.frame(x = xx, y = yy)
  else {
    data$x <- xx
    data$y <- yy
  }

  new_form <- "y ~ x"
  if (!is.null(form$condition))
    new_form <- paste(new_form, "|", paste(names(form$condition), sep = " + "))
  new_form <- as.formula(new_form)

  # Retrieve call
  ccall <- match.call()
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(ternaryplot)

  # Update call
  ccall$x <- new_form
  ccall$panel <- panel
  ccall$data <- data
  ccall$scales <- list(draw = FALSE)
  ccall$aspect <- "iso"
  ccall$xlab <- ""
  ccall$ylab <- ""
  ccall$xlim <- c(-0.2, 1.2)
  ccall$ylim <- c(-0.3, sqrt(3)/2 + 0.2)
  ccall$labs <- list(z = if (is.null(xlab)) form$left.name else xlab,
                     y = if (is.null(ylab)) form$right.y.name else ylab,
                     x = if (is.null(zlab)) form$right.x.name else zlab)

  # Remove bounding box if no strips
  if (!grepl("|", deparse(ccall$x), fixed = TRUE) || isTRUE(ccall$outer))
    ccall$par.settings$axis.line <- list(col = "transparent")

  # Make the call
  ccall[[1]] <- quote(lattice::xyplot)
  ans <- eval.parent(ccall)
  ans$call <- ocall

  # Make sure xlab etc are not plotted as usual
  ans
}

ternaryplot.data.frame <- function(x, data = NULL, ...) {
  if (!is.null(data))
    warning("Explicit 'data' specifications will be ignored.")

  ternaryplot(as.matrix(x), data = NULL, ...)
}

#' @describeIn ternaryplot A matrix of exactly three columns, wherein the the
#'   first column is treated as `z`, the second as `x`, and the third as `y`.
#' @export
ternaryplot.matrix <- function(x, data = NULL, ...) {
  if (ncol(x) != 3)
    stop("'x' needs to have exactly 3 columns.")
  names <- colnames(x)
  form <- paste(names[1], "~", names[2], "*", names[3])
  ternaryplot(form, data = NULL, ...)
}

#' Panel Function for Ternary Plots
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param z Numeric vector
#' @param grid Draw a reference grid.
#' @param subscripts See [lattice::panel.xyplot()].
#' @param labs A list of labels for the diagram, which may be in the form
#'   of expressions.
#' @param \dots Arguments passed on to [lattice::xyplot()].
#'
#' @inherit panel.bubbleplot return
#' @export
#'
#' @examples
panel.ternaryplot <- function(
    x,
    y,
    grid = TRUE,
    labs,
    density = TRUE,
    region = density,
    density_alpha = 0.25,
    ...
  ) {
  top <- sqrt(3)/2
  tck <- trellis.par.get("axis.components")$left$tck *
    lattice.getOption("axis.units")$outer$left$tick$x
  tck_unit <- lattice.getOption("axis.units")$outer$left$tick$units

  # Draw ticks and axis annotation
  m <- top / 0.5

  # Ticks size (can this be set using tck options from lattice?)
  tck_len <- 0.01

  breaks_x <- list(x = seq.int(0, 1, by = 0.2), y = rep.int(0, 6))
  breaks_y <- lapply(approx(x = c(0.5, 1), y = c(top, 0), n = 6), rev)
  breaks_z <- lapply(approx(x = c(0, 0.5), y = c(0, top), n = 6), rev)

  if (grid) {
    panel.segments(
      x0 = c(breaks_x$x, breaks_x$x, rev(breaks_z$x)),
      x1 = c(rev(breaks_y$x), rev(breaks_z$x), breaks_y$x),
      y0 = c(breaks_x$y, breaks_x$y, rev(breaks_z$y)),
      y1 = c(rev(breaks_y$y), rev(breaks_z$y), breaks_y$y),
      col = trellis.par.get("reference.line")$col
    )
  }

  # Ticks
  panel.segments(
    x0 = c(breaks_x$x,
           breaks_y$x,
           breaks_z$x),
    x1 = c(breaks_x$x - tck_len * tck,
           breaks_y$x + sqrt(tck_len^2 + (tck_len * m)^2) * tck,
           breaks_z$x - tck_len * tck),
    y0 = c(breaks_x$y,
           breaks_y$y,
           breaks_z$y),
    y1 = c(breaks_x$y - tck_len * m * tck,
           breaks_y$y,
           breaks_z$y + tck_len * m * tck)
  )

  # Pull in settings from lattice
  axis.text <- trellis.par.get("axis.text")
  xlab.text <- trellis.par.get("par.xlab.text")
  ylab.text <- trellis.par.get("par.ylab.text")
  zlab.text <- trellis.par.get("par.zlab.text")

  # Axes
  nudgex <- cos(60 * pi / 180) / 35
  nudgey <- sin(60 * pi / 180) / 35

  panel.text(
    cbind(c(breaks_x$x - tck_len * tck - nudgex,
            breaks_y$x + sqrt(tck_len^2 + (tck_len * m)^2) * tck +
              sqrt(nudgex^2 + nudgey^2),
            breaks_z$x - tck_len * tck - nudgex),
          c(breaks_x$y - tck_len * m * tck - nudgey,
            breaks_y$y,
            breaks_z$y + tck_len * m * tck + nudgey)),
    srt = rep(c(60, 0, -60), each = 6),
    pos = rep(c(2, 4, 2), each = 6),
    offset = 0,
    labels = rep(seq.int(1, 0, -0.2), 3),
    cex = trellis.par.get("axis.text")$cex,
    alpha = trellis.par.get("axis.text")$alpha,
    col = trellis.par.get("axis.text")$col,
    font = trellis.par.get("axis.text")$font
  )

  # Axis labels
  nudgex <- cos(30 * pi / 180) / 6
  nudgey <- sin(30 * pi / 180) / 6

  panel.text(
    x = cbind(c(0.5,
                0.75 + nudgex,
                0.25 - nudgex),
              c(0 - sqrt(nudgex^2 + nudgey^2),
                top/2 + nudgey,
                top/2 + nudgey)),
    srt = c(0, -60, 60),
    cex = c(xlab.text$cex, ylab.text$cex, zlab.text$cex),
    col = c(xlab.text$col, ylab.text$col, zlab.text$col),
    alpha = c(xlab.text$alpha, ylab.text$alpha, zlab.text$alpha),
    font = c(xlab.text$font, ylab.text$font, zlab.text$font),
    pos = c(1, 3, 3),
    offset = 0,
    labels = labs
  )

  # Density?
  if (density) {
    panel.2ddensity(x, y, region = region, ternary = TRUE, alpha = density_alpha)
  }

  # Draw the triangle
  panel.polygon(cbind(c(0, 1, 0.5, 0), c(0, 0, top, 0)))

  # Overlay points
  panel.xyplot(x, y, ...)
}

sign2 <- function(p1x, p1y, p2x, p2y, p3x, p3y) {
  (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)
}

#' Title
#'
#' @param x
#' @param y
#' @param n
#' @param lims
#' @param ternary
#' @param density_alpha
#'
#' @return
#' @export
#'
#' @examples
panel.2ddensity <- function(
    x,
    y,
    n = 100,
    region = TRUE,
    lims = if (ternary)
      c(0, 1, 0, sqrt(3)/2)
    else
      c(current.panel.limits()$xlim, current.panel.limits()$ylim),
    ternary = FALSE,
    alpha = trellis.par.get("regions")$alpha
  ) {
  dens <- MASS::kde2d(x, y, n = n, lims = lims)
  ll <- grDevices::contourLines(dens$x, dens$y, dens$z)
  if (region) {
    levels <- unique(vapply(ll, "[[", "level", FUN.VALUE = numeric(1)))
    fills <- trellis.par.get("regions")$col
    # Take the upper half of regions.col
    n <- length(fills)
    fills <- grDevices::colorRampPalette(fills[floor(n/2):n])(length(levels))
    names(fills) <- levels
  }

  for (i in seq_along(ll)) {
    xt <- ll[[i]]$x
    yt <- ll[[i]]$y
    if (ternary) {
      ii <- inside_triangle(xt, yt)
      dd <- cbind(xt[ii], yt[ii])
    } else {
      dd <- cbind(xt, yt)
    }

    if (region) {
      panel.polygon(dd,
                    col = fills[names(fills) == ll[[i]]$level],
                    alpha = alpha,
                    border = "transparent")
    }
    panel.lines(dd, alpha = alpha)
  }
}

# See if a point is inside a given triangle
inside_triangle <- function(x, y) {
  ax <- 0
  ay <- 0
  bx <- 1
  by <- 0
  cx <- 0.5
  cy <- sqrt(3)/2

  b1 <- sign2(x, y, ax, ay, bx, by) < 0
  b2 <- sign2(x, y, bx, by, cx, cy) < 0
  b3 <- sign2(x, y, cx, cy, ax, ay) < 0

  (b1 == b2) & (b2 == b3)
}
