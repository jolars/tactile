#' An extended box and whiskers plot
#'
#' An extended version of [lattice::bwplot()]. The only modification is to
#' group and stack box plots if `groups` is provided.
#'
#' @param x see [lattice::bwplot()]
#' @param data see [lattice::bwplot()]
#' @param ... arguments passed down to [lattice::bwplot()]
#' @param allow.multiple see [lattice::bwplot()]
#' @param outer see [lattice::bwplot()]
#' @param auto.key see [lattice::bwplot()]
#' @param groups see [lattice::bwplot()]
#' @param drop.unused.levels see [lattice::bwplot()]
#' @param subset see [lattice::bwplot()]
#' @param xlab see [lattice::bwplot()]
#'
#' @inherit bubbleplot return
#' @export
#'
#' @examples
#' bwplot2(variety ~ yield,
#'         groups = site,
#'         data = barley,
#'         par.settings = tactile.theme())
bwplot2 <- function(x, data = NULL, ...){
  UseMethod("bwplot2")
}

#' @rdname bwplot2
#' @export
bwplot2.formula <- function(
  x,
  data = NULL,
  allow.multiple = is.null(groups) || outer,
  outer = FALSE,
  auto.key = FALSE,
  groups = NULL,
  drop.unused.levels = lattice.getOption("drop.unused.levels"),
  ...,
  subset = TRUE
) {
  # Need to eval groups to set up plot in the high-level call
  groups <- eval(substitute(groups), data, environment(x))
  subset <- eval(substitute(subset), data, environment(x))
  form <- latticeParseFormula(x,
                              data,
                              subset = subset,
                              groups = groups,
                              multiple = allow.multiple,
                              outer = outer,
                              subscripts = TRUE,
                              drop = drop.unused.levels)
  groups <- form$groups

  # Retrieve call
  ccall <- match.call()
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(bwplot2)

  # Update call
  ccall$auto.key <- if (isTRUE(auto.key)) {
    list(rectangles = TRUE, points = FALSE)
  } else if (is.list(auto.key)) {
    updateList(list(rectangles = TRUE, points = FALSE), auto.key)
  } else {
    NULL
  }

  if (!is.null(groups)) {
    ccall$n_groups <- length(unique(groups))
    ccall$panel = panel.superpose
    if(is.null(ccall$box.width))
      ccall$box.width <- 1/(ccall$n_groups + 1)
    ccall$panel.groups <- function(x, y,
                                   horizontal,
                                   ...,
                                   pch = box.dot$pch,
                                   n_groups,
                                   group.number,
                                   box.width) {
      box.dot <- trellis.par.get("box.dot")
      n <- n_groups
      if (horizontal || is.null(horizontal)) {
        y <- y + (group.number - (n + 1)/2)*box.width
      } else {
        x <- x + (group.number - (n + 1)/2)*box.width
      }

      panel.bwplot(x = x,
                   y = y,
                   horizontal = horizontal,
                   box.width = box.width,
                   ...)
    }
  }

  # Make the call
  ccall[[1]] <- quote(lattice::bwplot)
  ans <- eval.parent(ccall)
  ans$call <- ocall
  ans
}

#' @rdname bwplot2
#' @export
bwplot2.numeric <- function(
  x,
  data = NULL,
  xlab = deparse(substitute(x)),
  ...
) {
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(bwplot2)
  ccall <- match.call()
  if (!is.null(ccall$data))
    warning("explicit 'data' specification ignored")
  ccall$data <- environment()
  ccall$xlab <- xlab
  ccall$x <- ~ x
  ccall[[1]] <- quote(lattice::bwplot)
  ans <- eval.parent(ccall)
  ans$call <- ocall
  ans
}
