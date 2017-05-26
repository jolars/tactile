# Wrapper around grid.arrange()
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
        args$ncol <- layout[1]
        args$nrow <- layout[2]
      }
    }
    do.call(grid.arrange, args)
    invisible(x)
  }
}

# rescale to new range
rescale <- function(x, new_min = 0, new_max = 1) {
  old_min <- min(x, na.rm = TRUE)
  old_max <- max(x, na.rm = TRUE)
  (x - old_min)/(old_max - old_min) * (new_max - new_min) + new_min
}

# suppress plot, return object
dont_plot <- function(x) {
  tmp <- tempfile()
  grDevices::png(tmp)
  p <- graphics::plot(x)
  grDevices::dev.off()
  unlink(tmp)
  invisible(p)
}

updateList <- function(x, val) {
  if (is.null(x))
    x <- list()
  if (!is.list(val))
    tryCatch(val <- as.list(val))
  if (!is.list(x))
    tryCatch(x <- as.list(x))
  modifyList(x, val)
}

# Retrieve function even if it is only named
get_fun <- function(fun) {
  if (is.function(fun))
    fun
  else if (is.character(fun))
    get(fun)
  else
    eval(fun)
}

# Convert a z ~ x * y formula to y ~ x
xyz_to_xy <- function(form) {
  new_form <- paste(form$right.y.name, "~", form$right.x.name)
  if (!is.null(form$condition))
    new_form <- paste(new_form, "|", paste(names(form$condition), sep = " + "))
  as.formula(new_form)
}

# Require packages that are only listed in Suggests
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(paste("Package ",  pkg, " is needed for this function to work. Please install it."),
         call. = FALSE)
}

# Borrowed from lattice
needAutoKey <- function(auto.key, groups = NULL) {
  (!is.null(groups) && (isTRUE(auto.key) || is.list(auto.key))) ||
     (is.list(auto.key) && !is.null(auto.key$text))
}
