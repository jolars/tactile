#' Lattice plot diagnostics for lm objects
#'
#' Lattice plot diagnostics for `lm` objects, mostly mimicking the behavior
#' of [stats::plot.lm()] but based on [lattice::xyplot()] instead.
#'
#' @inheritParams stats::plot.lm
#' @param which if a subset of the plots is required, specify a subset of the
#'   numbers `1:6`
#' @param main if `TRUE` plots default titles. Can also be a `list` or character
#'   vector of length 6.
#' @param data Only provided for method consistency and is ignored.
#' @param layout a numeric vector with `[columns, rows]` to use in the call
#'   to [gridExtra::grid.arrange()], or a layout matrix which will then be
#'   passed as the `layout_matrix` in `grid.arrange()`.
#' @param \dots arguments to be passed to [lattice::xyplot()].
#'
#' @return A list of `trellis` objects or a single `trellis` object.
#' @author Original by John Maindonald and Martin Maechler. Adaptation to
#'   lattice by Johan Larsson.
#'
#' @seealso [stats::lm()], [stats::plot.lm()], [lattice::xyplot()]
#'
#' @export
#'
#' @examples
#' fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' xyplot(fit)
#' xyplot(fit, which = 5)
xyplot.lm <-
  function(x,
           data = NULL,
           which = c(1:3, 5),
           main = FALSE,
           id.n = 3,
           labels.id = names(residuals(x)),
           cex.id = 0.75,
           cook.levels = c(0.5, 1),
           label.pos = c(4, 2),
           layout = NULL,
           ...) {
  if (!is.numeric(which) || any(which < 1) || any(which > 6))
    stop("'which' must be in 1:6")

  add.line <- trellis.par.get("add.line")
  reference.line <- trellis.par.get("reference.line")

  isGlm <- inherits(x, "glm")
  show <- rep(FALSE, 6)
  show[which] <- TRUE
  if (!(is.list(main) || is.character(main))) {
    main <- if (main)
      list(
        "Residuals vs Fitted",
        "Normal Q-Q",
        "Scale-Location",
        "Cook's distance",
        "Residuals vs Leverage",
        "Cook's distance vs Leverage"
      )
    else
      replicate(6, NULL)
  }

  plot_list <- vector("list", 6)

  r <- residuals(x)
  yh <- predict(x)
  w <- weights(x)

  if (!is.null(w)) {
    wind <- w != 0
    r <- r[wind]
    yh <- yh[wind]
    w <- w[wind]
    labels.id <- labels.id[wind]
  }

  n <- length(r)

  if (any(show[2:6])) {
    s <- if (inherits(x, "rlm"))
      x$s
    else if (isGlm)
      sqrt(summary(x)$dispersion)
    else
      sqrt(deviance(x) / df.residual(x))

    hii <- (infl <- influence(x, do.coef = FALSE))$hat

    if (any(show[4:6])) {
      cook <- if (isGlm)
        cooks.distance(x, infl = infl)
      else
        cooks.distance(x, infl = infl, sd = s, res = r, hat = hii)
    }
  }

  if (any(show[2:3])) {
    ylab23 <- if (isGlm)
      "Std. deviance resid."
    else
      "Standardized residuals"

    r.w <- if (is.null(w)) r else sqrt(w) * r
    rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
  }

  if (any(show[5:6])) {
    r.hat <- range(hii, na.rm = TRUE)
    isConst.hat <-
      all(r.hat == 0) || diff(r.hat) < 1e-10 * mean(hii, na.rm = TRUE)
  }

  if (any(show[c(1, 3)]))
    l.fit <- if (isGlm) "Predicted values" else "Fitted values"

  if (is.null(id.n)) {
    id.n <- 0
  } else {
    id.n <- as.integer(id.n)
    if (id.n < 0L || id.n > n)
      stop(gettextf("'id.n' must be in {1,..,%d}", n), domain = NA)
  }

  if (id.n > 0L) {
    if (is.null(labels.id))
      labels.id <- paste(1L:n)

    iid <- 1L:id.n
    show.r <- sort.list(abs(r), decreasing = TRUE)[iid]

    if (any(show[2:3]))
      show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]

    text.id <- function(x, y, ind, adj.x = TRUE) {
      labpos <- if (adj.x) label.pos[1 + as.numeric(x > mean(range(x)))] else 3
      panel.text(
        x = x,
        y = y,
        labels = labels.id[ind],
        cex = cex.id,
        pos = labpos
      )
    }
  }

  if (show[1L]) {
    ylim <- range(r, na.rm = TRUE)
    if (id.n > 0)
      ylim <- extendrange(r = ylim, f = 0.08)

    plot_list[[1]] <- xyplot(
      r ~ yh,
      xlab = l.fit,
      ylab = "Residuals",
      ylim = ylim,
      main = main[[1]],
      panel = function(x, y, ...) {
        panel.abline(h = 0,
                     col = reference.line$col,
                     lty = reference.line$lty,
                     lwd = reference.line$lwd,
                     alpha = reference.line$alpha)
        panel.xyplot(x, y, ...)
        if (id.n > 0) {
          y.id <- r[show.r]
          text.id(yh[show.r], y.id, show.r)
        }
        panel.loess(x, y, col = add.line$col, ...)
      },
      ...
    )
  }
  if (show[2L]) {
    qq <- qqnorm(rs, plot.it = FALSE)
    plot_list[[2]] <- qqmath(
      rs,
      main = main[[2]],
      ylab = ylab23,
      xlab = "Theoretical quantiles",
      panel = function(...) {
        panel.qqmathci(...)
        panel.qqmathline(...)
        panel.qqmath(...)
        if (id.n > 0)
          text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
      },
      ...
    )
  }
  if (show[3]) {
    sqrtabsr <- sqrt(abs(rs))
    if (is.null(w)) {
      yhn0 <- yh
    } else {
      yhn0 <- yh[w != 0]
    }

    plot_list[[3]] <- xyplot(
      sqrt(abs(rs)) ~ yhn0,
      xlab = l.fit,
      main = main[[3]],
      ylab = as.expression(substitute(sqrt(abs(YL)),
                                      list(YL = as.name(ylab23)))),
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.loess(x, y, col = add.line$col, ...)
        if (id.n > 0)
          text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
      },
      ...
    )
  }
  if (show[4]) {
    # Cooks distances
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      ymx <- cook[show.r[1L]] * 1.075
    } else {
      ymx <- max(cook, na.rm = TRUE)
    }

    plot_list[[4]] <- xyplot(
      cook ~ seq_along(cook),
      main = main[[4]],
      xlab = "Observation",
      ylab = "Cook's distance",
      ylim = c(-0.01, ymx * 1.1),
      type = "h",
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        if (id.n > 0)
          text.id(show.r, cook[show.r], show.r, adj.x = FALSE)
      },
      ...
    )

  }
  if (show[5]) {
    ylab5 <- if (isGlm)
      "Std. Pearson resid."
    else
      "Standardized residuals"

    r.w <- residuals(x, "pearson")

    if (!is.null(w))
      r.w <- r.w[wind]

    rsp <- dropInf(r.w / (s * sqrt(1 - hii)), hii)

    if (id.n > 0) {
      show.rsp <- order(-cook)[iid]
    }

    do.plot <- TRUE
    if (isConst.hat) {
      aterms <- attributes(terms(x))
      dcl <- aterms$dataClasses[-aterms$response]
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- model.frame(x)[facvars]

      if (ncol(mf) > 0) {
        dm <- data.matrix(mf)
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))

        ff <- if (nf == 1)
          1
        else
          rev(cumprod(c(1, nlev[nf:2])))

        facval <- (dm - 1) %*% ff
        xx <- facval

        plot_list[[5]] <- xyplot(
          rsp ~ factor(facval, labels = x$xlevels[[1]]),
          main = main[[5]],
          xlab = "Factor Level Combinations",
          ylab = ylab5,
          panel = function(x, y, ...) {
            panel.abline(h = 0, lty = reference.line$lty,
                         col = reference.line$col,
                         lwd = reference.line$lwd,
                         alpha = reference.line$alpha)
            panel.xyplot(x, y, ...)
            if (id.n > 0) {
              y.id <- rsp[show.rsp]
              text.id(xx[show.rsp], y.id, show.rsp)
            }
            panel.loess(x, y, col = add.line$col, ...)
          },
          ...
        )

      }
      else {
        message(gettextf("hat values (leverages) are all = %s\n and there are no factor predictors; no plot no. 5",
                         format(mean(r.hat))), domain = NA)
        do.plot <- FALSE
      }
    }
    else {
      xx <- hii
      xx[xx >= 1] <- NA

      if (length(cook.levels)) {
        p <- x$rank
        lms <- extendrange(xx, f = 0.1)
        ylim <- extendrange(rsp, f = 0.1)
        hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), lms[2L], length.out = 101)
        xmax <- min(0.99, lms[2L])
        ymult <- sqrt(p * (1 - xmax) / xmax)
        aty <- sqrt(cook.levels) * ymult
      }

      plot5_pars <- list(
        x = rsp ~ xx,
        main = main[[5]],
        xlab = "Leverage",
        ylab = ylab5,
        xlim = lms,
        ylim = ylim,
        scales = list(y = list(tck = c(2, 0))),
        key = list(corner = c(0, 0.02),
                   text = list("Cook's distance", col = "darkorange"),
                   lines = list(col = "darkorange", lty = 2)),
        panel = function(x, y, ...) {
          panel.abline(h = 0,
                       lty = reference.line$lty,
                       col = reference.line$col,
                       lwd = reference.line$lwd,
                       alpha = reference.line$alpha)
          panel.xyplot(x, y, ...)
          panel.loess(x, y, col = add.line$col, ...)
          if (id.n > 0) {
            y.id <- rsp[show.rsp]
            text.id(xx[show.rsp], y.id, show.rsp)
          }
          if (length(cook.levels)) {
            for (crit in cook.levels) {
              cl.h <- sqrt(crit * p * (1 - hh) / hh)
              ina <- cl.h < ylim[2]
              inb <- -cl.h > ylim[1]
              llines(hh[ina], cl.h[ina], lty = 2, col = "darkorange")
              llines(hh[inb], -cl.h[inb], lty = 2, col = "darkorange")
            }
            panel.axis(
              side = "right",
              at = c(-rev(aty), aty),
              labels = paste(c(rev(cook.levels), cook.levels)),
              text.col = "darkorange",
              outside = TRUE,
              ticks = FALSE,
              text.cex = cex.id
            )
          }
        }
      )

      plot5_pars <- updateList(plot5_pars, list(...))

      plot5_pars$par.settings$superpose.line <-
        list(lty = 2, col = "darkorange")
      plot5_pars$par.settings$clip <- list(panel = "off")
      plot5_pars$par.settings$layout.widths$right.padding <- 2

      plot_list[[5]] <- do.call(xyplot, plot5_pars)
    }
  }
  if (show[6]) {
    g <- dropInf(hii/(1 - hii), hii)
    ymx <- max(cook, na.rm = TRUE) * 1.025

    p <- x$rank
    bval <- pretty(sqrt(p * cook/g), 5)

    xlim <- c(0, max(g, na.rm = TRUE))
    ylim <- c(0, ymx)

    xrange <- extendrange(xlim, f = 0.1)
    yrange <- extendrange(ylim, f = 0.1)

    xmax <- xrange[2]
    ymax <- yrange[2]

    plot_list[[6]] <- xyplot(
      cook ~ g,
      ylim = yrange,
      xlim = xrange,
      main = main[[6]],
      ylab = "Cook's distance",
      xlab = expression("Leverage  " * h[ii]),
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        for (i in seq_along(bval)) {
          bi2 <- bval[i] ^ 2
          latticeExtra::panel.ablineq(
            a = 0,
            b = bi2,
            lty = reference.line$lty,
            col = reference.line$col,
            lwd = reference.line$lwd,
            alpha = reference.line$alpha,
            label = bval[i],
            rotate = TRUE,
            fontfamily = "sans",
            at = 0.95,
            cex = 0.8
          )
        }
        panel.loess(x, y)
        if (id.n > 0) {
          show.r <- order(-cook)[iid]
          text.id(g[show.r], cook[show.r], show.r)
        }
      },
      ...
    )
  }
  grid_wrap(plot_list, layout = layout)
}
