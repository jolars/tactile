# The following are, more or less, verbatim copies of internal functions in
# the lattice package (version 0.20-35). They came attached with the following
# copyright note.

### Copyright (C) 2001-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

trellis.skeleton <-
  function(formula = NULL,
           cond,
           aspect = default.args$aspect, # argument in xyplot
           as.table = default.args$as.table,
           between = default.args$between,
           key = NULL,
           legend = NULL,
           page = default.args$page,
           main = default.args$main,
           sub = default.args$sub,
           par.strip.text = default.args$par.strip.text,
           layout = default.args$layout,
           skip = default.args$skip,
           strip = default.args$strip.default, # argument in xyplot
           strip.left = FALSE,
           xlab.default = NULL,
           ylab.default = NULL,
           xlab = NULL, # argument in xyplot
           ylab = NULL, # argument in xyplot
           xlab.top = NULL,
           ylab.right = NULL,
           panel,       # argument in xyplot
           xscale.components = default.args$xscale.components,
           yscale.components = default.args$yscale.components,
           axis = default.args$axis,
           subscripts = TRUE, # ignored, for reasons given above
           index.cond = NULL,
           perm.cond = NULL,
           ...,
           par.settings = NULL,
           plot.args = NULL,
           lattice.options = NULL) {
    default.args <- lattice.getOption("default.args")
    if (is.null(skip)) skip <- FALSE
    foo <-
      list(formula = formula,
           as.table = as.table,
           aspect.fill = (aspect == "fill"),
           ## key = key,
           legend = construct.legend(legend = legend, key = key),
           panel = panel,
           page = page,
           layout = layout,
           skip = skip,
           strip = if (is.logical(strip) && strip) "strip.default"
           else strip,
           strip.left = if (is.logical(strip.left) && strip.left)
             strip.custom(horizontal = FALSE)
           else strip.left,
           xscale.components = xscale.components,
           yscale.components = yscale.components,
           axis = axis,
           xlab = xlab,
           ylab = ylab,
           xlab.default = xlab.default,
           ylab.default = ylab.default,
           xlab.top = xlab.top,
           ylab.right = ylab.right,
           main = main,
           sub = sub,
           x.between = 0,
           y.between = 0,
           par.settings = par.settings,
           plot.args = plot.args,
           lattice.options = lattice.options,
           par.strip.text = par.strip.text,
           index.cond = index.cond,
           perm.cond = perm.cond)

    if (!is.null(between$x)) foo$x.between <- between$x
    if (!is.null(between$y)) foo$y.between <- between$y

    foo$condlevels <- lapply(cond, levels)

    list(foo = foo, dots = list(...))
}

construct.legend <- function(legend = NULL, key = NULL, fun = "draw.key") {
  if (is.null(legend) && is.null(key))
    return(NULL)
  if (is.null(legend))
    legend <- list()
  if (!is.null(key)) {
    space <- key$space
    x <- y <- corner <- NULL
    if (is.null(space)) {
      if (any(c("x", "y", "corner") %in% names(key))) {
        stopifnot(is.null(x) || (length(x) == 1 && x >=
                                   0 && x <= 1))
        stopifnot(is.null(y) || (length(y) == 1 && y >=
                                   0 && y <= 1))
        stopifnot(is.null(corner) || (length(corner) ==
                                        2 && all(corner %in% c(0, 1))))
        space <- "inside"
        x <- key$x
        y <- key$y
        corner <- key$corner
      }
      else space <- "top"
    }
    if (space != "inside" && space %in% names(legend))
      stop(gettextf("component '%s' duplicated in key and legend",
                    space))
    key.legend <- list(fun = fun, args = list(key = key,
                                              draw = FALSE))
    key.legend$x <- x
    key.legend$y <- y
    key.legend$corner <- corner
    legend <- c(list(key.legend), legend)
    names(legend)[1] <- space
  }
  legend
}

updateList <- function(x, val) {
  if (is.null(x))
    x <- list()
  modifyList(x, val)
}

construct.scales <- function(draw = TRUE,
                             axs = "r",
                             tck = 1,
                             tick.number = 5,
                             at = FALSE,
                             labels = FALSE,
                             log = FALSE,
                             alternating = TRUE,
                             relation = "same",
                             abbreviate = FALSE,
                             minlength = 4,
                             limits = NULL,
                             format = NULL,
                             equispaced.log = TRUE,
                             lty = FALSE,
                             lwd = FALSE,
                             cex = FALSE,
                             rot = FALSE,
                             col = FALSE,
                             col.line = col,
                             alpha = FALSE,
                             alpha.line = alpha,
                             font = FALSE,
                             fontfamily = FALSE,
                             fontface = FALSE,
                             lineheight = FALSE,
                             ...,
                             x = NULL,
                             y = NULL) {
  x.scales <- y.scales <- list(draw = draw,
                               axs = axs,
                               tck = tck,
                               tick.number = tick.number,
                               at = at,
                               labels = labels,
                               log = log,
                               alternating = alternating,
                               relation = relation,
                               abbreviate = abbreviate,
                               minlength = minlength,
                               limits = limits,
                               format = format,
                               equispaced.log = equispaced.log,
                               lty = lty,
                               lwd = lwd,
                               cex = cex,
                               rot = rot,
                               col = col,
                               col.line = col.line,
                               alpha = alpha,
                               alpha.line = alpha.line,
                               font = font,
                               fontfamily = fontfamily,
                               fontface = fontface,
                               lineheight = lineheight)
  if (!is.null(x)) {
    if (is.character(x))
      x <- list(relation = x)
    x <- complete_names(x, x.scales)
    x.scales[names(x)] <- x
  }
  if (!is.null(y)) {
    if (is.character(y))
      y <- list(relation = y)
    y <- complete_names(y, y.scales)
    y.scales[names(y)] <- y
  }
  if (is.logical(x.scales$alternating))
    x.scales$alternating <- if (x.scales$alternating)
      c(1, 2)
  else 1
  if (is.logical(y.scales$alternating))
    y.scales$alternating <- if (y.scales$alternating)
      c(1, 2)
  else 1
  for (nm in c("tck", "cex", "rot")) {
    x.scales[[nm]] <- rep(x.scales[[nm]], length.out = 2)
    y.scales[[nm]] <- rep(y.scales[[nm]], length.out = 2)
  }
  if (x.scales$relation == "same" && (is.list(x.scales$at) ||
                                      is.list(x.scales$labels)))
    stop("the 'at' and 'labels' components of 'scales' may not be lists when 'relation = \"same\"'")
  if (y.scales$relation == "same" && (is.list(y.scales$at) ||
                                      is.list(y.scales$labels)))
    stop("the 'at' and 'labels' components of 'scales' may not be lists when 'relation = \"same\"'")
  list(x.scales = x.scales, y.scales = y.scales)
}

complete_names <- function(x, template, allow.invalid = FALSE) {
  pid <- pmatch(names(x), names(template), duplicates.ok = TRUE)
  if (allow.invalid) {
    x <- x[!is.na(pid)]
    pid <- pid[!is.na(pid)]
  }
  else {
    if (any(is.na(pid)))
      warning("Invalid or ambiguous component names: ",
              paste(names(x)[which(is.na(pid))], collapse = ", "))
  }
  if (any(duplicated(pid)))
    stop("Multiple matches to component name")
  names(x) <- names(template)[pid]
  x
}

compute.packet <- function(cond, levels) {
  id <- !(do.call("pmax", lapply(cond, is.na)))
  stopifnot(any(id))
  for (i in seq_along(cond)) {
    var <- cond[[i]]
    id <- id & (if (is.shingle(var))
      ((var >= levels(var)[[levels[i]]][1]) &
         (var <= levels(var)[[levels[i]]][2]))
    else
      (as.numeric(var) == levels[i]))
  }
  id
}

cupdate <- function(index, maxim) {
  if (length(index) != length(maxim) || length(maxim) <= 0)
    stop("Inappropriate arguments")
  index[1] <- index[1] + 1
  if (index[1] > maxim[1] && length(maxim) > 1)
    c(1, cupdate(index[-1], maxim[-1]))
  else index
}

limits.and.aspect <- function(prepanel.default,
                              prepanel = NULL,
                              have.xlim = FALSE,
                              xlim = NULL,
                              have.ylim = FALSE,
                              ylim = NULL,
                              x.relation,
                              y.relation,
                              panel.args.common = list(),
                              panel.args = list(),
                              aspect,
                              banking = lattice.getOption("banking"),
                              npackets = length(panel.args),
                              x.axs = "r",
                              y.axs = "r",
                              ...)
{
  prepanel.default.function <- getFunctionOrName(prepanel.default)
  prepanel <- getFunctionOrName(prepanel)
  if (npackets < 1)
    stop("need at least one panel")
  x.limits <- vector("list", npackets)
  y.limits <- vector("list", npackets)
  x.used.at <- vector("list", npackets)
  y.used.at <- vector("list", npackets)
  x.num.limit <- vector("list", npackets)
  y.num.limit <- vector("list", npackets)
  dxdy <- vector("list", npackets)
  for (count in seq_len(npackets)) {
    if (is.list(panel.args[[count]])) {
      pargs <- c(panel.args.common, panel.args[[count]],
                 list(...))
      tem <- do.call(prepanel.default.function, pargs)
      if (is.function(prepanel)) {
        prenames <- names(formals(prepanel))
        if (!("..." %in% prenames))
          pargs <- pargs[intersect(names(pargs), prenames)]
        pretem <- do.call("prepanel", pargs)
        if (!is.null(pretem$xlim) && !is.character(pretem$xlim))
          if (any(isna <- is.na(pretem$xlim)))
            pretem$xlim[isna] <- tem$xlim[isna]
        if (!is.null(pretem$ylim) && !is.character(pretem$ylim))
          if (any(isna <- is.na(pretem$ylim)))
            pretem$ylim[isna] <- tem$ylim[isna]
        tem <- updateList(tem, pretem)
      }
      x.limits[[count]] <- tem$xlim
      y.limits[[count]] <- tem$ylim
      x.used.at[[count]] <- if (is.null(tem$xat))
        NA
      else tem$xat
      y.used.at[[count]] <- if (is.null(tem$yat))
        NA
      else tem$yat
      x.num.limit[[count]] <- if (is.null(tem$xat))
        NA
      else range(tem$xat)
      y.num.limit[[count]] <- if (is.null(tem$yat))
        NA
      else range(tem$yat)
      dxdy[[count]] <- list(dx = tem$dx, dy = tem$dy)
    }
    else {
      x.limits[[count]] <- c(NA_real_, NA_real_)
      y.limits[[count]] <- c(NA_real_, NA_real_)
      x.used.at[[count]] <- NA_real_
      y.used.at[[count]] <- NA_real_
      x.num.limit[[count]] <- NA_real_
      y.num.limit[[count]] <- NA_real_
      dxdy[[count]] <- list(dx = NA_real_, dy = NA_real_)
    }
  }
  x.limits <- limitsFromLimitlist(
    have.lim = have.xlim,
    lim = xlim,
    relation = x.relation,
    limitlist = x.limits,
    used.at = x.used.at,
    numlimitlist = x.num.limit,
    axs = x.axs,
    npackets = npackets
  )
  y.limits <- limitsFromLimitlist(
    have.lim = have.ylim,
    lim = ylim,
    relation = y.relation,
    limitlist = y.limits,
    used.at = y.used.at,
    numlimitlist = y.num.limit,
    axs = y.axs,
    npackets = npackets
  )
  if (is.character(aspect)) {
    if (aspect == "xy") {
      aspect <-
        median(sapply(dxdy, banking) * y.limits$slicelen/x.limits$slicelen,
               na.rm = TRUE)
    }
    else if (aspect == "iso") {
      aspect <- median(y.limits$slicelen/x.limits$slicelen, na.rm = TRUE)
      if (y.relation == "free" || x.relation == "free")
        warning("'aspect=\"iso\"' approximate since 'relation=\"free\"'")
    }
    else aspect <- 1
  }
  list(
    x.limits = x.limits$limits,
    y.limits = y.limits$limits,
    x.used.at = x.limits$used.at,
    y.used.at = y.limits$used.at,
    x.num.limit = x.limits$numlimitlist,
    y.num.limit = y.limits$numlimitlist,
    aspect.ratio = aspect,
    prepanel.default = prepanel.default,
    prepanel = prepanel
  )
}

getFunctionOrName <- function(FUN) {
  if (is.function(FUN))
    FUN
  else if (is.character(FUN))
    get(FUN)
  else eval(FUN)
}

limitsFromLimitlist <- function(have.lim,
                                lim,
                                relation,
                                limitlist,
                                used.at,
                                numlimitlist,
                                axs, npackets) {
  if (relation == "same") {
    all.na <- unlist(lapply(limitlist, function(x) all(is.na(x))))
    class.lim <- lapply(limitlist[!all.na], class)
    limits <- unlist(limitlist)
    if (sum(!is.na(limits)) > 0) {
      if (is.character(limits)) {
        limits <- unique(limits[!is.na(limits)])
        slicelen <- diff(extend.limits(limits, axs = axs))
      }
      else {
        limits <- extend.limits(range(as.numeric(limits),
                                      finite = TRUE), axs = axs)
        slicelen <- diff(range(limits, finite = TRUE))
      }
      if (length(class.lim) > 0)
        class(limits) <- if (all(class.lim[[1]] == "integer"))
          "numeric"
      else class.lim[[1]]
    }
    else {
      limits <- c(0, 1)
      slicelen <- 1
    }
    if (have.lim) {
      if (is.list(lim))
        stop("limits cannot be a list when relation = same")
      old.limits <- limits
      limits <- lim
      if (!is.character(limits) && !is.character(old.limits)) {
        limits[is.na(limits)] <- old.limits[is.na(limits)]
      }
      slicelen <- if (is.character(limits))
        length(limits) + 2
      else diff(range(as.numeric(limits)))
    }
    ans <- list(limits = limits, slicelen = slicelen)
  }
  else if (relation == "sliced") {
    if (have.lim) {
      if (is.list(lim)) {
        limits <- rep(lim, length.out = npackets)
      }
      else warning("Explicitly specified limits ignored")
    }
    slicelen <- limitlist
    for (i in seq_along(limitlist)) {
      slicelen[[i]] <- if (!is.character(limitlist[[i]])) {
        if (any(is.finite(limitlist[[i]])))
          diff(range(as.numeric(limitlist[[i]]), finite = TRUE))
        else NA_real_
      }
      else if (!any(is.na(numlimitlist[[i]])))
        diff(range(as.numeric(numlimitlist[[i]])))
      else NA_real_
    }
    slicelen <- (if (axs == "i")
      1
      else 1 + 2 * lattice.getOption("axis.padding")$numeric) *
      max(unlist(slicelen), na.rm = TRUE)
    for (i in seq_along(limitlist)) {
      if (is.numeric(limitlist[[i]]))
        limitlist[[i]] <- extend.limits(limitlist[[i]],
                                        length = slicelen)
    }
    for (i in seq_along(numlimitlist)) {
      if (!all(is.na(numlimitlist[[i]])))
        numlimitlist[[i]] <- extend.limits(as.numeric(numlimitlist[[i]]),
                                           length = slicelen)
    }
    ans <- list(limits = limitlist,
                used.at = used.at,
                numlimitlist = numlimitlist,
                slicelen = slicelen)
  }
  else if (relation == "free") {
    if (have.lim) {
      if (!is.list(lim))
        lim <- list(lim)
      id <- which(sapply(limitlist, function(x) !all(is.na(x))))
      old.limitlist <- limitlist
      limitlist[id] <- lim
      which.null <- sapply(limitlist, is.null)
      limitlist[which.null] <- old.limitlist[which.null]
      for (i in seq_along(limitlist)) {
        if (!is.character(limitlist[[i]]) &&
            !is.character(old.limitlist[[i]])) {
          isna <- is.na(limitlist[[i]])
          limitlist[[i]][isna] <- old.limitlist[[i]][isna]
        }
      }
    }
    for (i in seq_along(limitlist)) {
      if (!all(is.na(limitlist[[i]])) && !is.character(limitlist[[i]]))
        limitlist[[i]] <- extend.limits(limitlist[[i]],
                                        axs = axs)
    }
    slicelen <- numeric(length(limitlist))

    for (i in seq_along(limitlist))
      slicelen[i] <- if (!is.character(limitlist[[i]]))
        diff(range(as.numeric(limitlist[[i]])))
      else if (!any(is.na(numlimitlist[[i]])))
        diff(range(numlimitlist[[i]]))
      else NA_real_

    ans <- list(limits = limitlist,
                used.at = used.at,
                numlimitlist = numlimitlist,
                slicelen = slicelen)
  }
  ans
}

extend.limits <- function(
  lim,
  length = 1,
  axs = "r",
  prop = if (axs == "i") 0 else lattice.getOption("axis.padding")$numeric) {
  if (all(is.na(lim)))
    NA_real_
  else if (is.character(lim)) {
    c(1, length(lim)) + c(-1, 1) * if (axs == "i")
      0.5
    else lattice.getOption("axis.padding")$factor
  }
  else if (length(lim) == 2) {
    if (lim[1] > lim[2]) {
      ccall <- match.call()
      ccall$lim <- rev(lim)
      ans <- eval.parent(ccall)
      return(rev(ans))
    }
    if (!missing(length) && !missing(prop))
      stop("'length' and 'prop' cannot both be specified")
    if (length <= 0)
      stop("'length' must be positive")
    if (!missing(length)) {
      prop <-
        (as.numeric(length) - as.numeric(diff(lim)))/(2 * as.numeric(diff(lim)))
    }
    if (lim[1] == lim[2])
      lim + 0.5 * c(-length, length)
    else {
      d <- diff(as.numeric(lim))
      lim + prop * d * c(-1, 1)
    }
  }
  else {
    print(lim)
    stop("improper length of 'lim'")
  }
}

needAutoKey <- function(auto.key, groups = NULL)
{
  ((!is.null(groups) && (isTRUE(auto.key) || is.list(auto.key))) ||
     (is.list(auto.key) && !is.null(auto.key$text)))
}

cond.orders <- function(foo, ...) {
  index.cond <- vector(mode = "list", length = length(foo$condlevels))
  for (i in seq_along(foo$condlevels))
    index.cond[[i]] <- seq_along(foo$condlevels[[i]])
  perm.cond <- seq_len(length(foo$condlevels))
  if (!is.null(foo$perm.cond)) {
    if (all(sort(foo$perm.cond) == perm.cond))
      perm.cond <- foo$perm.cond
    else stop("Invalid value of perm.cond")
  }
  if (!is.null(foo$index.cond)) {
    if (is.list(foo$index.cond) && length(foo$index.cond) ==
        length(index.cond)) {
      for (i in seq_along(foo$condlevels))
        index.cond[[i]] <- index.cond[[i]][foo$index.cond[[i]]]
    }
    else if (is.function(foo$index.cond)) {
      FUN <- foo$index.cond
      nplots <- length(foo$panel.args)
      panel.order <- numeric(nplots)
      for (count in seq_len(nplots)) {
        if (is.list(foo$panel.args[[count]])) {
          pargs <- c(foo$panel.args.common, foo$panel.args[[count]],
                     list(...))
          prenames <- names(formals(FUN))
          if (!("..." %in% prenames))
            pargs <- pargs[intersect(names(pargs), prenames)]
          panel.order[count] <- do.call("FUN", pargs)
        }
        else {
          is.na(panel.order) <- count
        }
      }
      dim(panel.order) <- sapply(foo$condlevels, length)
      for (i in seq_along(foo$condlevels))
        index.cond[[i]] <- order(apply(panel.order, i, mean, na.rm = TRUE))
    }
    else stop("Invalid value of index.cond")
  }
  list(index.cond = index.cond, perm.cond = perm.cond)
}

prepanel.null <- function() {
  list(xlim = rep(NA_real_, 2),
       ylim = rep(NA_real_, 2),
       dx = NA_real_,
       dy = NA_real_)
}

logLimits <- function(lim, base) {
  if (is.list(lim))
    lapply(lim, log, base)
  else log(lim, base)
}
