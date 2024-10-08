% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walkingPath.R
\name{walkingPath}
\alias{walkingPath}
\title{Compute walking path from case/landmark to nearest or selected pump.}
\usage{
walkingPath(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, case.set = "observed",
  location = "nominal", weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, include.landmarks = TRUE)
}
\arguments{
\item{origin}{Numeric. Vector of origin(s) (numeric or case/landmark name).}

\item{destination}{Numeric. Vector of destination(s) (numeric or landmark/pump name).}

\item{type}{Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.}

\item{vestry}{Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.}

\item{latlong}{Logical.}

\item{case.set}{Character. "observed" or "expected".}

\item{location}{Character. For cases and pumps. "anchor, "fatality" or "orthogonal.}

\item{weighted}{Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.}

\item{distance.unit}{Character. Unit of distance: "meter" or "yard".}

\item{time.unit}{Character. "hour", "minute", or "second".}

\item{walking.speed}{Numeric. Walking speed in km/hr.}

\item{include.landmarks}{Logical. Include landmarks as cases.}
}
\description{
Compute walking path from case/landmark to nearest or selected pump.
}
