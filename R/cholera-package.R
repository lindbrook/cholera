#' cholera
#'
#' Amend errors, augment data and aid analysis of John Snow's map of the 1854 London cholera outbreak.
#'
#' \itemize{
#' \item Fixes two sets of errors in Dodson and Tobler's 1992 digitization of Snow's map: 1) three misplaced cases/fatalities and 2) one missing road segment (part of Clifford Street).
#' \item "Unstacks" the data in two ways to make analysis and visualization easier and more meaningful.
#' \item Computes and visualizes "pump neighborhoods" based on Voronoi tessellation, Euclidean distance, and walking distance.
#' \item Overlay graphical elements and features like kernel density estimates, Voronoi diagrams, Snow's Broad Street neighborhood, and notable landmarks (John Snow's residence, the Lion Brewery, etc.) via `add*()` functions.
#' \item Includes a variety of functions to highlight specific cases, roads, pumps and paths.
#' \item Appends actual street names to roads data.
#' \item Includes the revised pump data used in the second version of Snow's map from the Vestry report, which includes the "correct" location of the Broad Street pump.
#' \item Adds two different aggregate time series fatalities data sets, taken from the Vestry report.
#' \item Support for parallel computation on Linux and macOS; limited support for Windows.
#' \item Version >= 0.9.0, offers provisional support for (georeferenced) longitude and latitude for practically all data and functions.
#' }
#'
#' To learn more, see the vignettes:
#'
#' \code{vignette("duplicate.missing.cases")}
#'
#' \code{vignette("kernel.density")}
#'
#' \code{vignette("parallelization")}
#'
#' \code{vignette("pump.neighborhoods")}
#'
#' \code{vignette("roads")}
#'
#' \code{vignette("tiles.polygons")}
#'
#' \code{vignette("time.series")}
#'
#' \code{vignette("unstacking.bars")}
#'
"_PACKAGE"
NULL
