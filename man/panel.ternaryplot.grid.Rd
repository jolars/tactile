% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternaryplot.R
\name{panel.ternaryplot.grid}
\alias{panel.ternaryplot.grid}
\title{Reference Grid for Ternary Plot}
\usage{
panel.ternaryplot.grid(
  at = seq.int(0, 1, by = 0.2),
  alpha = reference.line$alpha,
  col = reference.line$col,
  lty = reference.line$lty,
  lwd = reference.line$lwd
)
}
\arguments{
\item{at}{Where to draw the reference lines}

\item{alpha}{Alpha}

\item{col}{Color}

\item{lty}{Line type}

\item{lwd}{Line weight}
}
\value{
Plots a layer inside a panel of a \code{lattice} plot.
}
\description{
Reference Grid for Ternary Plot
}
