% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addKernelDensity.R
\name{addKernelDensity}
\alias{addKernelDensity}
\title{Add 2D kernel density contours.}
\usage{
addKernelDensity(pump.subset = "pooled", pump.select = NULL,
  neighborhood.type = "walking", data = "unstacked", bandwidth = 0.5,
  color = "black", line.type = "solid", multi.core = FALSE,
  latlong = FALSE)
}
\arguments{
\item{pump.subset}{Character or Numeric: "pooled", "individual", or numeric vector. "pooled" treats all observations as a single set. "individual" is a shortcut for all individual pump neighborhoods. Use of vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.}

\item{pump.select}{Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.}

\item{neighborhood.type}{Character. "voronoi" or "walking"}

\item{data}{Character. Unit of observation: "unstacked" uses \code{fatalities.unstacked}; "anchor" uses \code{fatalities.anchor}; "fatality" uses \code{fatalities}.}

\item{bandwidth}{Numeric. Bandwidth for kernel density estimation.}

\item{color}{Character. Color of contour lines.}

\item{line.type}{Character. Line type for contour lines.}

\item{multi.core}{Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.}

\item{latlong}{Logical.}
}
\value{
Add contours to a graphics plot.
}
\description{
Add 2D kernel density contours based on selected sets of observations.
}
\note{
This function uses \code{KernSmooth::bkde2D()}.
}
\examples{
\dontrun{
snowMap()
addKernelDensity()

snowMap()
addKernelDensity("individual")

snowMap()
addKernelDensity(c(6, 8))

snowMap()
addKernelDensity(pump.select = c(6, 8))
}
}
