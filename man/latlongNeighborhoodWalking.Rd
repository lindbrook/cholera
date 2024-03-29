% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latlongNeighborhoodWalking.R
\name{latlongNeighborhoodWalking}
\alias{latlongNeighborhoodWalking}
\title{Compute walking path pump neighborhoods.}
\usage{
latlongNeighborhoodWalking(pump.select = NULL, vestry = FALSE,
  case.set = "observed", weighted = TRUE, multi.core = TRUE)
}
\arguments{
\item{pump.select}{Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps. Note that you can't just select the pump on Adam and Eve Court (#2) because it's technically an isolate.}

\item{vestry}{Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.}

\item{case.set}{Character. "observed" or "expected".}

\item{weighted}{Logical. \code{TRUE} computes shortest path weighted by road length. \code{FALSE} computes shortest path in terms of the number of nodes.}

\item{multi.core}{Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.}
}
\description{
Group cases into neighborhoods based on walking distance.
}
