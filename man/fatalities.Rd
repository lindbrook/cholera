% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fatalities}
\alias{fatalities}
\title{Amended Dodson and Tobler's cholera data.}
\format{
A data frame with 3 variable that records the position and the nearest pump for the 578 bars on Snow's map.

 \describe{
    \item{\code{case}}{numeric case ID}
    \item{\code{x}}{x-coordinate}
    \item{\code{y}}{y-coordinate}
    \item{\code{lon}}{longitude}
    \item{\code{lat}}{latitude}
}
}
\usage{
fatalities
}
\description{
An amended version of Dodson and Tobler's digitization of John Snow's map of the 1854 London cholera outbreak. It removes 3 duplicate observations and imputes the location for 3 "missing" observation. This information is also available in HistData::Snow.deaths2 (>= ver. 0.7-8).
}
\note{
fixFatalities() documents the code for these data. For details, see \code{vignette}("duplicate.missing.cases").
}
\seealso{
\code{\link{caseLocator}}

\code{\link{streetNameLocator}}

\code{\link{streetNumberLocator}}

\code{\link{caseLocator}}

\code{\link{streetNameLocator}}

\code{\link{streetNumberLocator}}
}
\keyword{datasets}
