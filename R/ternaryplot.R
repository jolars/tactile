#' Ternary Plot
#'
#' A ternary diagram is a triangular diagram that displays proportions of
#' three variables.
#'
#' @param x See **Methods (by class)**.
#' @param data A data frame in which the `formula`, `groups`, and conditioning
#'   variables are evaluated.
#' @inheritParams panel.ternaryplot
#' @inheritParams lattice::levelplot
#' @param panel The default panel function used to create the display.
#' @param ... Arguments passed on to [lattice::cloud()].
#'
#' @inherit lattice::levelplot return
#'
#' @export
#'
#' @examples
#' ternaryplot(Fertility ~ Agriculture * Catholic, data = swiss)
#' ternaryplot(Catholic ~ Examination * Education, response = Infant.Mortality,
#'             data = swiss, contour = FALSE)
#'
#' xyz <- data.frame(x = runif(50), y = runif(50), z = runif(50),
#'                  f = gl(2, 25, labels = letters[1:2]))
#' ternaryplot(z ~ x * y | f, data = xyz, density = TRUE)
ternaryplot <- function(x, data, ...) {
  UseMethod("ternaryplot")
}

#' @describeIn ternaryplot A formula of the form `top ~ left * right`. Variables
#'   will be evaluated inside data if provided.
#' @export
ternaryplot.formula <- function(
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
  ...
) {
  response <- eval(substitute(response), data, environment(x))

  # Retrieve call
  ccall <- match.call()
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(ternaryplot)

  # Update call
  ccall$response <- response
  ccall$panel <- panel
  ccall$default.prepanel <- default.prepanel
  ccall$density <- density
  ccall$region <- region
  ccall$data <- data
  ccall$aspect <- "iso"
  ccall$xlim <- xlim
  ccall$ylim <- ylim

  # Make the call
  ccall[[1]] <- quote(lattice::cloud)
  ans <- eval.parent(ccall)
  ans$call <- ocall

  # Set up breaks for colorkey
  # Do we have to compute density again here (and how do we then make sure that
  # color coding is equivalent between panels)?
  if (density && (isTRUE(colorkey) || is.list(colorkey))) {
    iso <- with(ans$panel.args.common, ilr(x, y, z))
    dens <- MASS::kde2d(iso$x, iso$y, n = 100,
                        lims = c(-16.92048, 16.92048, -14.65356, 14.65356))
    density_levels <- pretty(dens$z, n = 10)
  }

  # Set up colorkey (if needed)
  ans$legend <- setup_key(
    ans$legend,
    colorkey,
    if (density)
      list(col = seq_pal(100), at = density_levels)
    else if (!is.null(response) && region)
      list(col = trellis.par.get("regions")$col,
           at = pretty(quantile(response, c(0.1, 0.9), 10)))
    else list(),
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
  if (NCOL(x) < 3)
    stop("'x' needs at least 3 columns.")
  if (!is.null(data))
    warning("Exlicit 'data' specification has been dropped.")

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
#' @param density Density estimation via [MASS::kde2()]
#' @param region Fill density or response estimates with a color gradient.
#' @param contour Draw contour lines for density and response estimates.
#' @param labels Label contour lines.
#' @param xlab X axis label (the left dimension)
#' @param ylab Y axis label (the right dimension)
#' @param zlab Z axis label (the top dimension)
#' @param plot_points Plot points
#' @param ... Arguments passed down to subsequent panel functions.
#' @param subscripts See [lattice::panel.xyplot()].
#'
#' @seealso [panel.ternaryplot.density, panel.ternaryplot.response]
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
    xlab,
    ylab,
    zlab,
    ...
  ) {
  # Pull in settings from lattice
  axis.components <- trellis.par.get("axis.components")
  axis.line <- trellis.par.get("axis.line")
  axis.text <- trellis.par.get("axis.text")
  axis.units <- lattice.getOption("axis.units")
  xlab.text <- trellis.par.get("par.xlab.text")
  ylab.text <- trellis.par.get("par.ylab.text")
  zlab.text <- trellis.par.get("par.zlab.text")
  reference.line <- trellis.par.get("reference.line")

  if (length(subscripts) == 0)
    return()

  xyz <- data.frame(x, y, z)[subscripts, ]
  lrt <- xyz / rowSums(xyz)
  xy <- tern_cart(lrt)

  top <- sqrt(3L)/2L
  tck <- axis.components$left$tck * axis.units$outer$left$tick$x
  tck_unit <- axis.units$outer$left$tick$units

  # Ticks size (can this be set using tck options from lattice?)
  tck_len <- 0.01

  breaks_x <- list(x = seq.int(0, 1, by = 0.2), y = rep.int(0, 6))
  breaks_y <- lapply(approx(x = c(0.5, 1), y = c(top, 0), n = 6), rev)
  breaks_z <- lapply(approx(x = c(0, 0.5), y = c(0, top), n = 6), rev)

  if (grid) {
    panel.segments(
      x0 = c(breaks_x$x,      breaks_x$x,      rev(breaks_z$x)),
      x1 = c(rev(breaks_y$x), rev(breaks_z$x), breaks_y$x),
      y0 = c(breaks_x$y,      breaks_x$y,      rev(breaks_z$y)),
      y1 = c(rev(breaks_y$y), rev(breaks_z$y), breaks_y$y),
      col = reference.line$col
    )
  }

  # Density?
  if (density)
    panel.ternaryplot.density(
      x = x,
      y = y,
      z = z,
      subscripts = subscripts,
      region = region,
      contour = contour,
      labels = labels,
      ...
    )
  else if (!is.null(response))
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

  # Clip around density
  ternary_clip(xl = current.panel.limits()$x, current.panel.limits()$y)

  # Ticks
  m <- sqrt(3L)

  panel.segments(
    x0 = c(breaks_x$x,
           breaks_y$x,
           breaks_z$x),
    x1 = c(breaks_x$x - tck_len * tck,
           breaks_y$x + sqrt(tck_len^2L + (tck_len * m)^2L) * tck,
           breaks_z$x - tck_len * tck),
    y0 = c(breaks_x$y,
           breaks_y$y,
           breaks_z$y),
    y1 = c(breaks_x$y - tck_len * m * tck,
           breaks_y$y,
           breaks_z$y + tck_len * m * tck)
  )

  # Axes
  nudgex <- cos(60L * pi / 180L) / 35L
  nudgey <- sin(60L * pi / 180L) / 35L

  panel.text(
    cbind(c(breaks_x$x - tck_len * tck - nudgex,
            breaks_y$x + sqrt(tck_len^2L + (tck_len * m)^2L) * tck +
              sqrt(nudgex^2L + nudgey^2L),
            breaks_z$x - tck_len * tck - nudgex),
          c(breaks_x$y - tck_len * m * tck - nudgey,
            breaks_y$y,
            breaks_z$y + tck_len * m * tck + nudgey)),
    srt = rep(c(60L, 0L, -60L), each = 6L),
    pos = rep(c(2L, 4L, 2L), each = 6L),
    offset = 0L,
    labels = rep(seq.int(1L, 0L, -0.2), 3L),
    cex = axis.text$cex,
    alpha = axis.text$alpha,
    col = axis.text$col,
    font = axis.text$font
  )

  # Axis labels
  nudgex <- cos(30L * pi / 180L) / 6L
  nudgey <- sin(30L * pi / 180L) / 6L

  panel.text(
    x = cbind(c(0.5,
                0.75 + nudgex,
                0.25 - nudgex),
              c(0L - sqrt(nudgex^2L + nudgey^2L),
                top/2L + nudgey,
                top/2L + nudgey)),
    srt = c(0L, -60L, 60L),
    cex = c(xlab.text$cex, ylab.text$cex, zlab.text$cex),
    col = c(xlab.text$col, ylab.text$col, zlab.text$col),
    alpha = c(xlab.text$alpha, ylab.text$alpha, zlab.text$alpha),
    font = c(xlab.text$font, ylab.text$font, zlab.text$font),
    pos = c(1L, 3L, 3L),
    offset = 0L,
    labels = list(xlab, ylab, zlab)
  )

  # Draw bounding triangle
  panel.polygon(
    cbind(c(0L, 1L, 0.5, 0L), c(0L, 0L, top, 0L)),
    border = if (axis.line$col == "transparent") 1L else axis.line$col,
    lwd = axis.line$lwd,
    alpha = axis.line$alpha,
    lty = axis.line$lty
  )

  # Overlay points
  if (points)
    panel.xyplot(x = xy[, 1L], y = xy[, 2L], subscripts = subscripts, ...)
}

#' Response Panels for Ternary Plots
#'
#' @inheritParams panel.ternaryplot
#' @param fun Function to apply to the response variable.
#' @param formula Formula for the function.
#' @param ... Arguments passed on to [lattice::panel.lines()],
#'   [lattice::panel.polygon()], [lattice::panel.text()].
#'
#' @inherit panel.bubbleplot
#' @export
panel.ternaryplot.response <- function(
    x,
    y,
    z,
    subscripts,
    response,
    region = TRUE,
    contour = TRUE,
    labels = contour,
    fun = c("glm", "lm"),
    formula = response ~ poly(x, y),
    ...
  ) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(z))

  if (missing(response))
    stop("`panel.ternaryplot.response` needs a response variable.")

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
    fills <- level.colors(x = seq_len(numcol) - 0.5,
                          at = seq_len(numcol + 1L) - 1L,
                          col.regions = regions$col)

    for (i in seq_along(clines)[-length(clines)]) {
      a <- clines[[i + 1]]$xy
      b <- clines[[i]]$xy
      polys <- rbind(a, b[rev(seq_along(b$x)), ])

      do.call(panel.polygon, updateList(list(
        polys,
        border = "transparent",
        col = fills[i],
        lty = add.line$lty,
        lwd = add.line$lwd
      ), list(...)))
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
        mid <- which.min(abs(cumsum(as) - sum(as)/2L))

        xx <- xy$x[c(mid - 1L, mid + 1L)]
        xx <- xy$y[c(mid - 1L, mid + 1L)]

        dy <- diff(xy$y[c(mid + 1L, mid - 1L)])
        dx <- diff(xy$x[c(mid + 1L, mid - 1L)])
        angle <- atan2(dy, dx) * 180L/pi

        if (abs(angle) > 90L)
          angle <- angle - 180L

        do.call(panel.text, updateList(list(
          x = xy$x[mid],
          y = xy$y[mid],
          adj = c(0.5, 0L),
          labels = foo$level, srt = angle
        ), list(...)))
      }
      do.call(panel.lines, updateList(list(
        xy,
        border = "transparent",
        col = add.line$col,
        lty = add.line$lty,
        lwd = add.line$lwd
      ), list(...)))
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
    labels = TRUE,
    ...
  ) {
  regions <- trellis.par.get("regions")
  add.line <- trellis.par.get("add.line")

  # Run once to retrieve the same breaks as in the high-level function
  iso <- ilr(x, y, z)
  lims <- c(-16.92048, 16.92048, -14.65356, 14.65356)
  dens <- MASS::kde2d(iso$x, iso$y, n = n, lims = lims)
  density_levels <- pretty(dens$z, n = 10L)

  iso <- iso[subscripts, ]
  dens <- MASS::kde2d(iso$x, iso$y, n = n, lims = lims)
  clines <- with(dens, grDevices::contourLines(x, y, z, levels = density_levels))

  clines <- lapply(clines, function(x) {
    list(level = x$level, xy = tern_cart(ilr_inv(x$x, x$y)))
  })

  if (region) {
    levels <- vapply(clines, "[[", "level", FUN.VALUE = numeric(1))
    numcol <- length(levels) - 1L
    fills <- level.colors(x = seq_len(numcol) - 0.5,
                          at = seq_len(numcol + 1L) - 1L,
                          col.regions = seq_pal(100L))

    for (i in seq_along(clines)) {
      polys <- clines[[i]]$xy
      do.call(panel.polygon, updateList(list(
        polys,
        border = "transparent",
        col = fills[i],
        alpha = regions$alpha
      ), list(...)))
    }
  }

  if (contour) {
    for (i in seq_along(clines)) {
      foo <- clines[[i]]
      xy <- foo$xy

      if (labels && NROW(xy) > 5L) {
        ad <- vapply(xy, diff, FUN.VALUE = double(nrow(xy) - 1L))
        as <- sqrt(rowSums(ad^2L))
        mid <- which.min(abs(cumsum(as) - sum(as)/2L))

        xx <- xy$x[c(mid - 1L, mid + 1L)]
        xx <- xy$y[c(mid - 1L, mid + 1L)]

        dy <- diff(xy$y[c(mid + 1L, mid - 1L)])
        dx <- diff(xy$x[c(mid + 1L, mid - 1L)])
        angle <- atan2(dy, dx) * 180L/pi

        if (abs(angle) > 90L) angle <- angle - 180L

        do.call(panel.text, updateList(list(
          x = xy$x[mid],
          y = xy$y[mid],
          adj = c(0.5, 0L),
          labels = foo$level,
          srt = angle
        ), list(...)))
      }
      do.call(panel.lines, updateList(list(
        xy,
        border = "transparent",
        col = add.line$col,
        lty = add.line$lty,
        lwd = add.line$lwd
      ), list(...)))
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
#' @param ... Arguments that are passed on to [lattice::panel.xyplot()].
#'
#' @inherit panel.bubbleplot return
#' @export
panel.ternaryplot.xyplot <- function(x, y, z, ...) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(z))

  xyz <- data.frame(x, y, z)
  lrt <- xyz / rowSums(xyz)
  xy <- tern_cart(lrt)

  panel.xyplot(xy$x, xy$y, ...)
}

# Convert from ternary to cartesian coordinates
tern_cart <- function(l, r = NULL, t = NULL) {
  if (NCOL(l) > 1L) {
    r <- l[, 2L]
    t <- l[, 3L]
    l <- l[, 1L]
  }
  x <- 0.5 * (2L * r + t) / (l + r + t)
  y <- sqrt(3L)/2L * t / (l + r + t)
  data.frame(x, y)
}

# Convert from cartesian to ternary coordinates
cart_tern <- function(x, y = NULL) {
  if (NCOL(x) > 1L) {
    y <- x[, 2L]
    x <- x[, 1L]
  }
  r <- x - y * tan(pi/6L)
  t <- y / (tan(pi/3L) * 0.5)
  l <- 1L - r - t
  data.frame(l, r, t)
}

# Isometric log-ratio transformation
ilr <- function(x, y = NULL, z = NULL) {
  if (any(x < 0L))
    stop("No negative values, please.")

  if (!is.null(y) || !is.null(z))
    x <- data.frame(x, y, z)

  data.frame(
    x = -sqrt(2L/3L) *
      log(exp(rowSums(log(as.matrix(x[, c(2L, 3L)])))/2L) / x[, 1L]),
    y = -sqrt(0.5) * log(x[, 3L] / x[, 2L])
  )
}

# Inverse isometric log-ratio transformation
ilr_inv <- function(x, y = NULL) {
  if (!is.null(y))
    x <- data.frame(x, y)

  x <- -x
  n <- nrow(x)
  xyz <- data.frame(x = double(n), y = double(n), z = double(n))

  xyz[, 1L] <- -sqrt(2/3) * x[, 1L]
  xyz[, 2L] <- x[, 1L] / sqrt(6)
  xyz[, 3L] <- xyz[, 2L] + x[, 2L] / sqrt(2L)
  xyz[, 2L] <- xyz[, 2L] - x[, 2L] * sqrt(0.5)

  exp(xyz) / rowSums(exp(as.matrix(xyz)))
}

# Clip around ternary plot
ternary_clip <- function(xl, yl) {
  polx <- c(xl[1], 0,       0.5, 1, xl[1], xl[1], xl[2], xl[2], xl[1], xl[1])
  poly <- c(0,     0, sqrt(3)/2, 0,     0, yl[1], yl[1], yl[2], yl[2],     0)
  panel.polygon(polx, poly, col = "white", border = "transparent")
}

