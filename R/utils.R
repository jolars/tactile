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
