# Wrapper around grid.arrange()
grid_wrap <- function(x, layout = NULL) {
  x <- x[!vapply(x, is.null, FUN.VALUE = logical(1))]

  if (length(x) == 1) {
    stats::update(x[[1]])
  } else {
    ll <- list(grobs = x)
    if (!is.null(layout)) {
      if (is.matrix(layout)) {
        ll$layout_matrix <- layout
      } else {
        ll$ncol <- layout[1]
        ll$nrow <- layout[2]
      }
    }
    do.call(gridExtra::grid.arrange, ll)
    invisible(x)
  }
}

# Wrapper for modifyList
update_list <- function(x, val) {
  if (is.null(x))
    x <- list()
  utils::modifyList(x = x, val = val)
}

# dropInf utlitiy function
dropInf <- function(x, h) {
  if (any(isInf <- h >= 1)) {
    warning(gettextf("Not plotting observations with leverage one:\n  %s",
                     paste(which(isInf), collapse = ", ")),
            call. = FALSE,
            domain = NA)
    x[isInf] <- NaN
  }
  x
}
