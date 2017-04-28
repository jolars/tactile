#' Lattice plot diagnostics for lm objects
#'
#' Lattice plot diagnostics for `lm` objects, mostly mimicking the behavior
#' of [stats::plot.lm()] but based on [lattice::xyplot()] instead.
#'
#' @inheritParams stats::plot.lm
#' @param which if a subset of the plots is required, specify a subset of the
#'   numbers `1:6`
#' @param main title to each plot
#' @param nrow rows in the resulting gTable.
#' @param ncol columns in the resulting gTable.
#' @param data Only provided for method consistency and is ignored.
#' @param \dots Arguments to be passed to [lattice::xyplot()].
#'
#' @return A `gTable` of trellis grobs or a single trellis object.
#'
#' @seealso [stats::lm()], [stats::plot.lm()], [lattice::xyplot()]
#'
#' @export
#'
#' @examples
#' library(latticework)
#' fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' xyplot(fit)
xyplot.lm <- function(x,
                      data = NULL,
                      which = c(1:3, 5),
                      main = list("Residuals vs Fitted",
                                  "Normal Q-Q",
                                  "Scale-Location",
                                  "Cook's distance",
                                  "Residuals vs Leverage",
                                  "Cook's distance vs Leverage"),
                      id.n = 3,
                      labels.id = names(stats::residuals(x)),
                      cex.id = 0.75,
                      cook.levels = c(0.5, 1),
                      label.pos = c(4, 2),
                      nrow = NULL,
                      ncol = NULL,
                      ...) {
  if (!is.numeric(which) || any(which < 1) || any(which > 6))
    stop("'which' must be in 1:6")

  isGlm <- inherits(x, "glm")
  show <- rep(FALSE, 6)
  show[which] <- TRUE

  plot_list <- vector("list", sum(show))

  r <- stats::residuals(x)
  yh <- stats::predict(x)
  w <- stats::weights(x)

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
     sqrt(stats::deviance(x) / stats::df.residual(x))

    hii <- (infl <- stats::influence(x, do.coef = FALSE))$hat

    if (any(show[4:6])) {
      cook <- if (isGlm)
        stats::cooks.distance(x, infl = infl)
      else
        stats::cooks.distance(x, infl = infl, sd = s, res = r, hat = hii)
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
      lattice::panel.text(
        x = x,
        y = y,
        labels = labels.id[ind],
        cex = cex.id,
        pos = labpos,
        offset = 0.25
      )
    }
  }

  if (show[1L]) {
    ylim <- range(r, na.rm = TRUE)
    if (id.n > 0)
      ylim <- grDevices::extendrange(r = ylim, f = 0.08)

    plot_list[[1]] <- lattice::xyplot(
      r ~ yh,
      xlab = l.fit,
      ylab = "Residuals",
      ylim = ylim,
      main = main[[1]],
      panel = function(x, y, ...) {
        lattice::panel.abline(h = 0, col = "gray50", lty = 3)
        lattice::panel.xyplot(x, y, ...)
        if (id.n > 0) {
          y.id <- r[show.r]
          text.id(yh[show.r], y.id, show.r)
        }
        lattice::panel.loess(x, y, ...)
      }
    )
  }
  if (show[2L]) {
    qq <- stats::qqnorm(rs, plot.it = FALSE)
    plot_list[[2]] <- lattice::qqmath(
      rs,
      main = main[[2]],
      ylab = ylab23,
      xlab = "Theoretical quantiles",
      panel = function(x, ...) {
        lattice::panel.qqmathline(x, lty = 3, col = "gray50", ...)
        lattice::panel.qqmath(x, ...)
        if (id.n > 0)
          text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
      }
    )
  }
  if (show[3]) {
    sqrtabsr <- sqrt(abs(rs))
    if (is.null(w)) {
      yhn0 <- yh
    } else {
      yhn0 <- yh[w != 0]
    }

    plot_list[[3]] <- lattice::xyplot(
      sqrt(abs(rs)) ~ yhn0,
      xlab = l.fit,
      main = main[[3]],
      ylab = as.expression(substitute(sqrt(abs(YL)),
                                      list(YL = as.name(ylab23)))),
      panel = function(x, y, ...) {
        lattice::panel.xyplot(x, y, ...)
        lattice::panel.loess(x, y, ...)
        if (id.n > 0)
          text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
      }
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

    plot_list[[4]] <- lattice::xyplot(
      cook ~ seq_along(cook),
      main = main[[4]],
      xlab = "Observation",
      ylab = "Cook's distance",
      ylim = c(0, ymx * 1.1),
      type = "h",
      panel = function(x, y, ...) {
        lattice::panel.xyplot(x, y, ...)
        if (id.n > 0)
          text.id(show.r, cook[show.r], show.r, adj.x = FALSE)
      }
    )

  }
  if (show[5]) {
    ylab5 <- if (isGlm) "Std. Pearson resid." else "Standardized residuals"
    r.w <- stats::residuals(x, "pearson")

    if (!is.null(w))
      r.w <- r.w[wind]

    rsp <- dropInf(r.w / (s * sqrt(1 - hii)), hii)

    if (id.n > 0) {
      show.rsp <- order(-cook)[iid]
    }

    do.plot <- TRUE
    if (isConst.hat) {
      aterms <- attributes(stats::terms(x))
      dcl <- aterms$dataClasses[-aterms$response]
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- stats::model.frame(x)[facvars]

      if (ncol(mf) > 0) {
        dm <- data.matrix(mf)
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))

        ff <- if (nf == 1)
          1
        else
          rev(cumprod(c(1, nlev[nf:2])))

        facval <- (dm - 1) %*% ff
        xx <- facval

        plot_list[[5]] <- lattice::xyplot(
          rsp ~ factor(facval, labels = x$xlevels[[1]]),
          main = main[[5]],
          xlab = "Factor Level Combinations",
          ylab = ylab5,
          panel = function(x, y, ...) {
            lattice::panel.abline(h = 0, lty = 3, col = "gray50")
            lattice::panel.xyplot(x, y, ...)
            if (id.n > 0) {
              y.id <- rsp[show.rsp]
              text.id(xx[show.rsp], y.id, show.rsp)
            }
            lattice::panel.loess(x, y, ...)
          }
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
        lms <- grDevices::extendrange(xx, f = 0.1)
        ylim <- grDevices::extendrange(rsp, f = 0.1)
        hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), lms[2L], length.out = 101)
        xmax <- min(0.99, lms[2L])
        ymult <- sqrt(p * (1 - xmax) / xmax)
        aty <- sqrt(cook.levels) * ymult
      }

      plot_list[[5]] <- lattice::xyplot(
        rsp ~ xx,
        main = main[[5]],
        xlab = "Leverage",
        ylab = ylab5,
        xlim = lms,
        ylim = ylim,
        scales = list(y = list(tck = c(2, 0))),
        par.settings = list(superpose.line = list(lty = 2, col = "orange"),
                            clip = list(panel = "off")),
        panel = function(x, y, ...) {
          lattice::panel.abline(h = 0, lty = 3, col = "gray50")
          lattice::panel.xyplot(x, y, ...)
          lattice::panel.loess(x, y, ...)
          if (id.n > 0) {
            y.id <- rsp[show.rsp]
            text.id(xx[show.rsp], y.id, show.rsp)
          }
          if (length(cook.levels)) {
            for (crit in cook.levels) {
              cl.h <- sqrt(crit * p * (1 - hh) / hh)
              ina <- cl.h < ylim[2]
              inb <- -cl.h > ylim[1]
              lattice::llines(hh[ina], cl.h[ina], lty = 2, col = "orange")
              lattice::llines(hh[inb], -cl.h[inb], lty = 2, col = "orange")
            }
            latticeExtra::panel.key(
              text = "Cook's distance",
              corner = c(0.0, 0.02),
              lines = TRUE,
              points = FALSE
            )
            lattice::panel.axis(
              side = "right",
              at = c(-rev(aty), aty),
              labels = paste(c(rev(cook.levels), cook.levels)),
              line.col = "orange",
              text.col = "orange",
              outside = TRUE,
              ticks = FALSE,
              text.cex = cex.id
            )
          }
        }
      )
    }
  }
  if (show[6]) {
    g <- dropInf(hii/(1 - hii), hii)
    ymx <- max(cook, na.rm = TRUE) * 1.025

    p <- x$rank
    bval <- pretty(sqrt(p * cook/g), 5)

    xlim <- c(0, max(g, na.rm = TRUE))
    ylim <- c(0, ymx)

    xrange <- grDevices::extendrange(xlim, f = 0.1)
    yrange <- grDevices::extendrange(ylim, f = 0.1)

    xmax <- xrange[2]
    ymax <- yrange[2]

    plot_list[[6]] <- lattice::xyplot(
      cook ~ g,
      ylim = yrange,
      xlim = xrange,
      main = main[[6]],
      ylab = "Cook's distance",
      xlab = expression("Leverage  " * h[ii]),
      panel = function(x, y, ...) {
        lattice::panel.xyplot(x, y, ...)
        for (i in seq_along(bval)) {
          bi2 <- bval[i] ^ 2
          latticeExtra::panel.ablineq(
            a = 0,
            b = bi2,
            lty = 3,
            col = "gray50",
            label = bval[i],
            rotate = TRUE,
            fontfamily = "sans",
            at = 0.9,
            cex = 0.8
          )

        }
        if (id.n > 0) {
          show.r <- order(-cook)[iid]
          text.id(g[show.r], cook[show.r], show.r)
        }
      }
    )
  }

  plot_list <- plot_list[!sapply(plot_list, is.null)]

  if (length(plot_list) == 1) {
    stats::update(plot_list[[1]])
  } else {
    grid_opts <- list()
    grid_opts$grobs <- plot_list
    if (!is.null(nrow))
      grid_opts$nrow <- nrow
    if (!is.null(ncol))
      grid_opts$ncol <- ncol

    do.call(gridExtra::grid.arrange, grid_opts)
  }
}


# dropInf utlitiy function
dropInf <- function(x, h) {
  if (any(isInf <- h >= 1)) {
    warning(gettextf("not plotting observations with leverage one:\n  %s",
                     paste(which(isInf), collapse = ", ")),
            call. = FALSE,
            domain = NA)
    x[isInf] <- NaN
  }
  x
}


