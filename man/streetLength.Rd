% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/streetLength.R
\name{streetLength}
\alias{streetLength}
\title{Compute length of selected street.}
\usage{
streetLength(road = "Oxford Street", distance.unit = "meter",
  latlong = FALSE)
}
\arguments{
\item{road}{Character or Numeric. Road name or number. For names, the function tries to correct for case and to remove extra spaces.}

\item{distance.unit}{Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on conversion.}

\item{latlong}{Logical. Use estimated longitude and latitude.}
}
\value{
An R vector of length one.
}
\description{
Compute length of selected street.
}
\examples{
streetLength("Oxford Street")
streetLength("oxford street")
streetLength("oxford street", distance.unit = "yard")
}
