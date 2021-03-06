% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel.ci.R
\name{panel.ci}
\alias{panel.ci}
\title{Panel function for confidence interval}
\usage{
panel.ci(x, y, lower, upper, groups = NULL, subscripts, col, fill = if
  (is.null(groups)) plot.line$col else superpose.line$col, alpha = 0.15,
  lty = 0, lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
  grid = FALSE, ..., col.line = if (is.null(groups)) plot.line$col else
  superpose.line$col)
}
\arguments{
\item{x}{ variables to be plotted in the scatterplot}

\item{y}{ variables to be plotted in the scatterplot}

\item{lower}{lower confidence limits}

\item{upper}{upper confidence limits}

\item{groups}{ an optional grouping variable.  If present,
    \code{\link{panel.superpose}} will be used instead to display each
    subgroup }

\item{subscripts}{see \code{\link[lattice:xyplot]{lattice::xyplot()}}}

\item{col}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}.
  }

\item{fill}{
    other graphical parameters.  \code{fill} serves the purpose of
    \code{bg} in \code{\link{points}} for certain values of \code{pch}
  }

\item{alpha}{opacity for the fill}

\item{lty}{
    other graphical parameters.  \code{fill} serves the purpose of
    \code{bg} in \code{\link{points}} for certain values of \code{pch}
  }

\item{lwd}{
    other graphical parameters.  \code{fill} serves the purpose of
    \code{bg} in \code{\link{points}} for certain values of \code{pch}
  }

\item{grid}{
    A logical flag, character string, or list specifying whether and how
    a background grid should be drawn.  This provides the same
    functionality as \code{type="g"}, but is the preferred alternative
    as the effect \code{type="g"} is conceptually different from that of
    other \code{type} values (which are all data-dependent).  Using the
    \code{grid} argument also allows more flexibility.

    Most generally, \code{grid} can be a list of arguments to be
    supplied to \code{\link{panel.grid}}, which is called with those
    arguments.  Three shortcuts are available:
    \describe{
      \item{\code{TRUE}:}{ roughly equivalent to \code{list(h = -1, v = -1)} }
      \item{\code{"h"}:}{ roughly equivalent to \code{list(h = -1, v = 0)} }
      \item{\code{"v"}:}{ roughly equivalent to \code{list(h = 0, v = -1)} }
    }
    No grid is drawn if \code{grid = FALSE}.
  }

\item{...}{
    Extra arguments, if any, for \code{panel.xyplot}.  In most cases
    \code{panel.xyplot} ignores these.  For types "r" and "smooth",
    these are passed on to \code{panel.lmline} and \code{panel.loess}
    respectively.
  }

\item{col.line}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}.
  }
}
\description{
Panel function for confidence interval
}
\examples{
mod <- lm(Petal.Width ~ Petal.Length*Species, data = iris)
newdat <- expand.grid(Petal.Length = seq(1, 7, by = 0.1),
                      Species = c("setosa", "versicolor", "virginica"))
pred <- predict(mod, newdat, interval = "confidence")
dd <- cbind(newdat, pred)

xyplot(fit ~ Petal.Length, groups = Species, data = dd,
       prepanel = prepanel.ci, auto.key = list(lines = TRUE, points = FALSE),
       ylab = "Petal Width",
       xlab = "Petal Length",
       lower = dd$lwr, upper = dd$upr, type = "l",
       panel = function(...) {
         panel.ci(..., alpha = 0.15, grid = TRUE)
         panel.xyplot(...)
       })

}
