% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fatalities.anchor}
\alias{fatalities.anchor}
\title{"Unstacked" fatalities data with "anchor" case as the unit of observation.}
\format{
A data frame with 4 variables for 321 addresses
 \describe{
    \item{\code{anchor}}{numerical case ID of address}
    \item{\code{x}}{x-coordinate}
    \item{\code{y}}{y-coordinate}
    \item{\code{case.count}}{number of fatalities at address}
    \item{\code{lon}}{longitude}
    \item{\code{lat}}{latitude}
}
}
\usage{
fatalities.anchor
}
\description{
An "unstacked" version of the \code{fatalities} dataset. It changes the unit of observation from the case (idividual bars) to the "anchor", the case at the base of a stack. The number of fatalities in a stack becomes an attribute of the"anchor".
}
\note{
\code{\link{unstackFatalities}} documents the code for these data. For details, see \code{vignette}("unstacking.fatalities").
}
\seealso{
\code{\link{caseLocator}}

\code{\link{streetNameLocator}}

\code{\link{streetNumberLocator}}
}
\keyword{datasets}
