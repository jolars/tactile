#' Wrapper for grid.arrange
#'
#' @param x List of trellis objects.
#' @param layout A layout matrix or vector specifying rows and columns
#'
#' @return A list of `trellis` objects.
#' @keywords internal
grid_wrap <- function(x, layout = NULL) {
  x <- x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  if (length(x) == 1) {
    update(x[[1]])
  } else {
    args <- list(grobs = x)
    if (!is.null(layout)) {
      if (is.matrix(layout)) {
        args$layout_matrix <- layout
      } else {
        args$ncol <- layout[1L]
        args$nrow <- layout[2L]
      }
    }
    do.call(gridExtra::grid.arrange, args)
    invisible(x)
  }
}

#' Uniform Rescaling
#'
#' @param x A numeric vector to rescale
#' @param new_min New min
#' @param new_max New max
#' @param old_min Old min
#' @param old_max Old max
#'
#' @return A rescaled version of `x`.
#' @keywords internal
rescale <- function(x,
                    new_min = 0,
                    new_max = 1,
                    old_min = min(x, na.rm = TRUE),
                    old_max = max(x, na.rm = TRUE)) {
  (x - old_min)/(old_max - old_min) * (new_max - new_min) + new_min
}


#' Suppress Plotting
#'
#' @param x Object to call [graphics::plot()] on.
#'
#' @return Invisibly returns whatever `plot(x)` would normally returns, but
#'   does not plot anything (which is the point).
#' @keywords internal
dont_plot <- function(x, ...) {
  tmp <- tempfile()
  png(tmp)
  p <- plot(x, ...)
  dev.off()
  unlink(tmp)
  invisible(p)
}

#' Update a List with User Input
#'
#' Wrapper for [utils::modifyList()].
#'
#' @param x A list to be updated.
#' @param val Stuff to update `x` with.
#'
#' @seealso [utils::modifyList()]
#'
#' @return Returns an updated list.
#' @keywords internal
updateList <- function(x, val) {
  if (is.null(x))
    x <- list()
  if (!is.list(val))
    tryCatch(val <- as.list(val))
  if (!is.list(x))
    tryCatch(x <- as.list(x))
  modifyList(x, val)
}


#' Retrieve a Function by Name or Definition
#'
#' @param fun Character or function.
#' @keywords internal
get_fun <- function(fun) {
  if (is.function(fun))
    fun
  else if (is.character(fun))
    get(fun)
  else
    eval(fun)
}


#' Convert a formula from z ~ x * y to y ~ x
#'
#' @param form Three-dimensional lattice formula.
#'
#' @return A two-dimensional lattice formula.
#' @keywords internal
xyz_to_xy <- function(form) {
  new_form <- paste(form$right.y.name, "~", form$right.x.name)
  if (!is.null(form$condition))
    new_form <- paste(new_form, "|", paste(names(form$condition), sep = " + "))
  as.formula(new_form)
}


#' Throw An Error if A Required Package Is Unavailable
#'
#' @param pkg The required package
#'
#' @return An error if the package namespace is not available.
#'
#' @keywords internal
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(paste("Package",
               pkg,
               "is needed for this function to work. Please install it."),
         call. = FALSE)
}


#' Sequential palette helper.
#'
#' Divides the regions palette from lattice in half when it does not make
#' sense to have a diverging palette.
#'
#' @param n Number of colors to generate
#' @param ... Stuff to pass on to [grDevices::colorRampPalette()]
#' @keywords internal
seq_pal <- function(n, bias = 1, space = "Lab", ...) {
  col <- trellis.par.get("regions")$col
  m <- length(col)
  col <- col[ceiling(m/2L):m]
  grDevices::colorRampPalette(col, bias = bias, space = space, ...)(n)
}

#' Key Setup
#'
#' Try to setup a key while also dodging existing keys
#'
#' @param legend a list of legends, usually the `$legend` slot of a trellis
#'   object.
#' @param key A key specification, usually the user input.
#' @param default_key The default key specifications that may be overridden by
#'   the user.
#' @param fun The function to draw the key, such as [lattice::draw.colorkey()].
#' @param pos Preferences for the position of the new key.
#'
#' @return The original `legend` object with the addition of the key defined by
#'   `key`, `default_key`, and `fun`.
#' @keywords internal
setup_key <- function(
    legend,
    key,
    default_key,
    fun,
    pos = c("right", "top", "bottom", "left")
  ) {
  if (isTRUE(key) || is.list(key)) {
    key <- list(
      fun = fun,
      args = list(
        key = updateList(default_key, if (is.list(key)) key else list())
      )
    )
    space <- if (!is.null(key$args$key$space))
      key$args$key$space
    else if (!is.null(legend)) {
      ii <- pos %in% names(legend)
      pos[!ii][1]
    } else
      "right"

    legend[[space]] <- key
  }
  legend
}
