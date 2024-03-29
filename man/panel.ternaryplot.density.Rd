% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ternaryplot.R
\name{panel.ternaryplot.density}
\alias{panel.ternaryplot.density}
\title{Two-Dimensional Density Estimation for Ternary Plots}
\usage{
panel.ternaryplot.density(
  x,
  y,
  z,
  subscripts,
  n = 100,
  region = TRUE,
  contour = FALSE,
  labels = isTRUE(contour),
  density_breaks = NULL,
  ...
)
}
\arguments{
\item{x}{Numeric vector}

\item{y}{Numeric vector}

\item{z}{Numeric vector}

\item{subscripts}{See \code{\link[lattice:panel.xyplot]{lattice::panel.xyplot()}}.}

\item{n}{
Number of grid points in each direction.  Can be scalar or a length-2
integer vector.
}

\item{region}{Fill density or response estimates with a color gradient.}

\item{contour}{Draw contour lines for density and response estimates.}

\item{labels}{Label contour lines.}

\item{density_breaks}{Breaks for the density plot if used (see
\code{\link[=panel.ternaryplot.density]{panel.ternaryplot.density()}}).}

\item{...}{Arguments that will be passed on to \code{\link[lattice:llines]{lattice::panel.lines()}},
\code{\link[lattice:llines]{lattice::panel.polygon()}}, and \code{\link[lattice:llines]{lattice::panel.text()}}.}
}
\value{
Plots a layer inside a panel of a \code{lattice} plot.
}
\description{
Two-Dimensional Density Estimation for Ternary Plots
}
