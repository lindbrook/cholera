#' cholera: amend, augment and aid analysis of John Snow's cholera data
#'
#' Amend, augment and aid the analysis of John Snow's cholera data.
#'
#' Features:
#'
#' \itemize{
#' \item Fixes three apparent coding errors in Dodson and Tobler's 1992 digitization of Snow's map.
#' \item "Unstacks" the data in two ways to improve analysis and visualization.
#' \item Computes pump neighborhoods based on Voronoi tessellation and walking distance.
#' \item Adds the ability to overlay graphical features like kernel density, Voronoi diagrams, and notable landmarks (the plague pit, the Lion Brewery, etc.).
#' \item Includes a variety of helper functions to find and locate cases, roads, pumps and walking paths.
#' \item Appends street names to roads data.
#' \item Includes the revised pump data used in the second version of Snow's map.
#' \item Adds two different aggregate time series fatalities data from the Vestry report.
#' }
#' To learn more, start with the vignettes:
#'
#' \code{vignette("duplicate.missing.cases")}
#'
#' \code{vignette("unstacking.fatalities")}
#'
#' \code{vignette("pump.neighborhoods")}
#'
#' \code{vignette("roads")}
#'
#' \code{vignette("time.series")}
#' @docType package
#' @name cholera-package
NULL
