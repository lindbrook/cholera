% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addIndexCase.R
\name{addIndexCase}
\alias{addIndexCase}
\title{Highlight index case at 40 Broad Street.}
\usage{
addIndexCase(latlong = FALSE, cex = 2, col = "red", pch = 1,
  add.label = FALSE, text.size = 0.5)
}
\arguments{
\item{latlong}{Logical.}

\item{cex}{Numeric. Size of point.}

\item{col}{Character. Color of point.}

\item{pch}{Numeric. Type of of point.}

\item{add.label}{Logical. Add text annotation: "40 Broad Street"}

\item{text.size}{Numeric. Size of text label.}
}
\value{
Add base R point and (optionally) text to a graphics plot.
}
\description{
Highlight index case at 40 Broad Street.
}
\examples{
segmentLocator("216-1")
addIndexCase()
}
