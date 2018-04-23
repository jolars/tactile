# This function has been imported from stats.plot.lm()
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
