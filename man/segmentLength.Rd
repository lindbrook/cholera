% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentLength.R
\name{segmentLength}
\alias{segmentLength}
\title{Compute length of road segment.}
\usage{
segmentLength(id = "216-1", distance.unit = "meter", latlong = FALSE)
}
\arguments{
\item{id}{Character. A concatenation of a street's numeric ID, a whole number between 1 and 528, and a second number used to identify the sub-segments.}

\item{distance.unit}{Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion. latlong = TRUE only returns meters.}

\item{latlong}{Logical.}
}
\value{
An R vector of length one.
}
\description{
Compute length of road segment.
}
\examples{
segmentLength("242-1")
segmentLength("242-1", distance.unit = "yard")
}
