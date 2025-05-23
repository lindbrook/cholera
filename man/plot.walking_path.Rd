% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walkingPath.R
\name{plot.walking_path}
\alias{plot.walking_path}
\title{Plot the walking path between selected cases and/or pumps.}
\usage{
\method{plot}{walking_path}(x, zoom = TRUE, add = FALSE,
  long.title = TRUE, mileposts = TRUE, milepost.unit = "distance",
  milepost.interval = NULL, alpha.level = 1, ...)
}
\arguments{
\item{x}{An object of class "walking_path" created by walkingPath().}

\item{zoom}{Logical or Numeric. Positive numbers zoom in; negative numbers zoom out.}

\item{add}{Logical. Add graphic to plot.}

\item{long.title}{Logical. Tile with names.}

\item{mileposts}{Logical. Plot mile/time posts.}

\item{milepost.unit}{Character. "distance" or "time".}

\item{milepost.interval}{Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).}

\item{alpha.level}{Numeric. Alpha level transparency for path: a value in [0, 1].}

\item{...}{Additional plotting parameters.}
}
\value{
A base R plot.
}
\description{
Plot the walking path between selected cases and/or pumps.
}
