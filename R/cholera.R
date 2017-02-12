#' Introduction
#'
#' Amend, augment and aid the analysis of John Snow's cholera data.
#'
#' Features:
#'
#' \itemize{
#' \item Fixes three apparent coding error in Dodson and Tobler's digitization of the data in Snow's map.
#' \item "Unstacks" the data in two ways to improve analysis and visualization.
#' \item Computes two types of pump neighborhoods: one based on Voronoi tesselation; one based on walking distance.
#' \item Adds the ability to overlay graphical features including kernel density, Voronoi diagrams, Snow's annotations of the Broad Street pump neighborhood, and notable landmarks (the plague pit, John Snow's residence, the Lion Brewery, etc.) onto your plot.
#' \item Includes a variety of helper functions to find and locate cases, roads, pumps and walking paths.
#' \item Appends real names to roads data.
#' \item Includes the revised pump data that was used to create the second version of Snow's map included in the Vestry report.
#' \item Adds two different aggregate time series fatalities data from the Vestry report
#' }
# #' To learn more, start with the vignettes:
# #' \code{browseVignettes(package = "cholera")}
#'
#' @docType package
#' @name cholera
NULL
