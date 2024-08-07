% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/euclideanNominal.R
\name{plot.euclidean}
\alias{plot.euclidean}
\title{Plot method for neighborhoodEuclidean().}
\usage{
\method{plot}{euclidean}(x, type = "star", add.observed.points = TRUE,
  add.title = TRUE, ...)
}
\arguments{
\item{x}{An object of class "euclidean" created by \code{neighborhoodEuclidean()}.}

\item{type}{Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.}

\item{add.observed.points}{Logical. Add observed fatality "addresses".}

\item{add.title}{Logical. Add title.}

\item{...}{Additional plotting parameters.}
}
\value{
A base R plot.
}
\description{
Plot method for neighborhoodEuclidean().
}
\note{
This uses an approximate computation of polygons, using the 'TSP' package, that may produce non-simple and/or overlapping polygons.
}
\examples{
\dontrun{
plot(neighborhoodEuclidean())
plot(neighborhoodEuclidean(-6))
plot(neighborhoodEuclidean(pump.select = 6:7))
plot(neighborhoodEuclidean(case.set = "expected"), type = "area.points")
plot(neighborhoodEuclidean(case.set = "expected"), type = "area.polygons")
}
}
