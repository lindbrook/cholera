#' Anchor or base case of each stack of fatalities.
#'
#' Data frame that links a case to its stack. which is the case at the base of a stack. For use with \code{\link{fatalityLocator}}.
#'
#' @format
#'  \describe{
#'     \item{\code{case}}{numerical case ID}
#'     \item{\code{anchor.case}}{numerical case ID of anchor.case}
#' }
#' @docType data
"anchor.case"

#' Map border line segments IDs.
#'
#' Vector of ordered numbers that identify the line segments that make up the frame of the map. For use with sp::Polygon().
#'
#' @format
#'  \describe{
#'     \item{\code{border}}{numerical ID}
#' }
#' @docType data
"border"

#' Amended Dodson and Tobler's cholera data.
#'
#' An amended version of Dodson and Tobler's digitization of John Snow's map of the 1854 London cholera outbreak. It removes 3 duplicate observations and imputes the location for 3 "missing" observation. This information is also available in HistData::Snow.deaths2 (>= ver. 0.7-8).
#'
#' @format A data frame with 3 variable that records the position and the nearest pump for the 578 bars on Snow's map.
#'
#'  \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#' }
#' @docType data
#' @seealso \code{vignette}("duplicate.missing.cases")
#'
#' \code{\link{fatalityLocator}}
#'
#' \code{\link[cholera]{streetNameViewer}}
#'
#' \code{\link[cholera]{streetNumberViewer}}
"fatalities"

#' "Unstacked" Dodson and Tobler's cholera data: addresss as unit of observation.
#'
#' An "unstacked" version of the \code{fatalities} dataset. It changes the unit of observation from the case (bar) to the "address", the x-y coordinate of the case at the base of a stack, and makes the number of fatalities an attribute of the "address".
#'
#' @format A data frame with 4 variables for 321 addresses
#'  \describe{
#'     \item{\code{anchor.case}}{numerical case ID of address}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'     \item{\code{case.count}}{number of fatalities at address}
#' }
#' @docType data
#' @seealso \code{vignette}("duplicate.missing.cases")
#'
#' \code{vignette}("unstacking.fatalities")
#'
#' \code{\link{fatalityLocator}}
#'
#' \code{\link[cholera]{streetNameViewer}}
#'
#' \code{\link[cholera]{streetNumberViewer}}
"fatalities.address"

#' "Unstacked" Dodson and Tobler's cholera fatalities data.
#'
#' An "unstacked" version of the \code{fatalities} dataset. It changes the unit of observation from the case (bar) to the "address", the x-y coordinate of the case at the base of a stack, and assigns the base case's coordinates to all cases in the stack.
#'
#' @format A data frame with 3 variable that records the position of the 578 bars on Snow's map.
#'
#'  \describe{
#'     \item{\code{case}}{numerical case ID}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#' }
#' @docType data
#' @seealso \code{vignette}("duplicate.missing.cases")
#'
#' \code{vignette}("unstacking.fatalities")
#'
#' \code{\link{fatalityLocator}}
#'
#' \code{\link[cholera]{streetNameViewer}}
#'
#' \code{\link[cholera]{streetNumberViewer}}
"fatalities.unstacked"

#' Orthogonal projection of observed cases onto road network.
#'
#' @format A data frame with 5 variable that records the position of the orthogonal projection of the 578 cases onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{case}}{numeric case ID}
#'  }
"ortho.proj"

#' Orthogonal projection of "expected" cases onto road network.
#'
#' @format A data frame with 5 variable that records the position of the orthogonal projection of the 4993 cases onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{case}}{numeric case ID}
#'  }
"ortho.proj.sp"

#' Plague pit coordinates
#'
#' Coordinates for polygon() or sp::Polygon(). In progress.
#' @format A data frame with 13 observations and 2 variables.
#' \describe{
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @docType data
"plague.pit"

#' Dodson and Tobler's pump data with road location.
#'
#' Adds and amends road locations for water pumps from John Snow's map to Dodson and Tobler's street data. The latter are available at Michael Friendly's HistData::Snow.streets.
#'
#' @format A data frame with 13 observations and 4 variables that describe the pumps on Snow's map.
#' \describe{
#'   \item{\code{id}}{pump number between 1 and 13}
#'   \item{\code{street}}{nearest street}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @docType data
"pumps"

#' Vestry report pump data.
#'
#' From the version Snow includes in the Vestry Report, this data set adds the Hanover Square pump and moves the Broad Street pump.
#'
#' @format A data frame with 14 observations and 4 variables that describe the pumps on the Vestry Report version of Snow's map. Amends Broad Street pump location.
#' \describe{
#'   \item{\code{id}}{pump number between 1 and 14}
#'   \item{\code{street}}{nearest street}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @docType data
"pumps.vestry"

#' Dodson and Tobler's street data with appended road names.
#'
#' Adds road names from John Snow's map to Dodson and Tobler's street data. The latter are available at Michael Friendly's HistData::Snow.streets.
#'
#' @format A data frame with 206 observations and 5 variables that describe the straight line segments used to recreate the roads on Snow's map.
#' \describe{
#'   \item{\code{street}}{street segment number, which range between 1 and 528}
#'   \item{\code{n}}{number of points in this street line segment}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#'   \item{\code{id}}{unique numeric ID}
#'   \item{\code{name}}{road name}
#' }
#' @docType data

#' @seealso \code{vignette}("road.names")
#'
#' \code{\link[cholera]{streetNameViewer}}
#'
#' \code{\link[cholera]{streetNumberViewer}}
"roads"

#' Snow Time Series Data.
#'
#' Time series of Fatalities from Snow's contribution to the Vertry Report (p. 117). Useful for studying the effect of removing the handle on the Broad Street pump on 08 September 1854.
#'
#' @format A data frame with 43 observations and 3 variables.
#' \describe{
#'   \item{\code{date}}{calendar date}
#'   \item{\code{fatalities}}{fatal attacks}
#'   \item{\code{deaths}}{daily deaths}
#' }
#' @docType data
"snow.time.series"

#' Vestry Report Time Series Data.
#'
#' Time series of Fatalities from Vertry Report Appendix B. Useful for studying the effect of removing the handle on the Broad Street pump on 08 September 1854.
#'
#' @format A data frame with 61 observations and 3 variables.
#' \describe{
#'   \item{\code{date}}{calendar date}
#'   \item{\code{fatalities}}{fatal attacks}
#'   \item{\code{deaths}}{deaths}
#' }
#' @docType data
"vestry.time.series"
