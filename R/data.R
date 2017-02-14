#' Anchor or base case of each stack of fatalities.
#'
#' Data frame that links a case to its stack. which is the case at the base of a stack. For use with \code{\link{caseLocator}}.
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
#' \code{\link{caseLocator}}
#'
#' \code{\link[cholera]{streetNameLocator}}
#'
#' \code{\link[cholera]{streetNumberLocator}}
"fatalities"

#' "Unstacked" Dodson and Tobler's cholera data: address as unit of observation.
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
#' \code{\link{caseLocator}}
#'
#' \code{\link[cholera]{streetNameLocator}}
#'
#' \code{\link[cholera]{streetNumberLocator}}
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
#' \code{\link{caseLocator}}
#'
#' \code{\link[cholera]{streetNameLocator}}
#'
#' \code{\link[cholera]{streetNumberLocator}}
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
#'  #' \code{\link{caseLocator}}
#' @docType data
"ortho.proj"

#' Orthogonal projection of "expected" cases onto road network.
#'
#' @format A data frame with 5 variable that records the position of the orthogonal projection of the 4993 cases onto the network of roads. Excluding isolates (Pump 2 at  Adam and Eve Court; Falconberg Court and Falconberg Mews), there are 4972 cases.
#' \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{case}}{numeric case ID}
#'  }
#' @seealso \code{\link{simulatedCases}}
#' @docType data
"ortho.proj.sp"

#' Orthogonal projection of 13 original pumps.
#'
#' @format A data frame with 5 variable that records the position of the orthogonal projection of the 13 original pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{pump.id}}{numeric ID}
#'  }
#' @docType data
"ortho.proj.pump"

#' Orthogonal projection of the 14 pumps from the Vestry Report.
#'
#' @format A data frame with 5 variable that records the position of the orthogonal projection of the 14 pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{pump.id}}{numeric ID}
#'  }
#' @docType data
"ortho.proj.pump.vestry"

#' Plague pit coordinates.
#'
#' Coordinates for polygon() or sp::Polygon(). In progress.
#' @format A data frame with 13 observations and 2 variables.
#' \describe{
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @docType data
"plague.pit"

#' List of the 578 observed cholera cases grouped by pump neighborhood.
#'
#' @format A list with 13 vectors.
#' \describe{
#'  \item{\code{pump.cases}}{numerical ID}
#' }
"pump.cases"

#' List of 4972 regular "simulated" cases grouped by pump neighborhood.
#'
#' @format A list with 13 vectors.
#' \describe{
#'  \item{\code{pump.cases.sp}}{numerical ID}
#' }
#' @docType data
"pump.cases.sp"

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
#' From the map Snow includes in the Vestry Report, this data set adds the pump at Hanover Square and relocates the Broad Street pump.
#'
#' @format A data frame with 14 observations and 4 variables.
#' \describe{
#'   \item{\code{id}}{pump number between 1 and 14}
#'   \item{\code{street}}{nearest street}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @docType data
"pumps.vestry"

#' "Expected" cases.
#'
#' The result of using sp::spsample() and sp::Polygon() to generate 5000 regularly spaced simulated cases within the map's border.
#'
#' @format A data frame with 2 variable that records the position of 4993 "expected" cases fitted by sp::spsample().
#'  \describe{
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'  }
#' @docType data
#' @seealso \code{\link{simulatedCases}}
"regular.cases"

#' Dodson and Tobler's street data transformed into road segments.
#'
#' This data set transforms Dodson and Tobler's street data to give each straight line segment of a "road" a unique ID.
#'
#' @format A data frame with 657 observations and 7 variables. The data describe the straight line segments used to recreate the roads on Snow's map.
#' \describe{
#'   \item{\code{street}}{numeric street ID, which range between 1 and 528}
#'   \item{\code{id}}{character segement ID}
#'   \item{\code{name}}{road name}
#'   \item{\code{x1}}{x-coordinate of first endpoint}
#'   \item{\code{y1}}{y-coordinate of first endpoint}
#'   \item{\code{x2}}{x-coordinate of second endpoint}
#'   \item{\code{y2}}{y-coordinate of second endpoint}
#' }
#' @docType data
"road.segments"

#' Dodson and Tobler's street data with appended road names.
#'
#' This data set adds road names from John Snow's map to Dodson and Tobler's street data. The latter are also available from HistData::Snow.streets.
#'
#' @format A data frame with 206 observations and 5 variables. The data describe the roads on Snow's map.
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
#' \code{\link[cholera]{streetNameLocator}}
#'
#' \code{\link[cholera]{streetNumberLocator}}
"roads"

#' Snow neighborhood cases.
#'
#' Data frame of the numeric IDs of cases, taken from from Dodson and Tobler, that fall within Snow's Broad Street pump neighborhood.
#'
#' @format A vector with 384 observations.
#' \describe{
#'   \item{\code{snow.neighborhood}}{numeric case ID}
#' }
#' @docType data
"snow.neighborhood"

#' "Trimmed" Snow neighborhood road segments.
#'
#' The road segments (i.e., endpoints) that describe Snow's Broad Street pump neighborhood (Vestry Report 1855b). Note that "outer" segments are trimmed: only the portion of the segment transversed by the walking path of the most "distant" case on that segment is included.
#'
#' @format A data frame with 6 variables and 384 observations.
#' \describe{
#'   \item{\code{road.segment}}{road segment ID}
#'   \item{\code{x1}}{first x-coordinate}
#'   \item{\code{y1}}{first y-coordinate}
#'   \item{\code{x2}}{second x-coordinate}
#'   \item{\code{y2}}{second y-coordinate}
#'   \item{\code{trimmed}}{trimmed road segment}
#' }
#' @docType data
"snow.trimmed.segments"

#' Vestry Report Time Series Data.
#'
#' Time series of recorded fatalities from Vestry Report Appendix B. Useful for studying the effect of removing the handle on the Broad Street pump on 08 September 1854.
#'
#' @format A data frame with 61 observations and 3 variables.
#' \describe{
#'   \item{\code{date}}{calendar date}
#'   \item{\code{fatalities}}{fatal attacks}
#'   \item{\code{deaths}}{deaths}
#' }
#' @docType data
"time.series.vestry"

#' Snow time series data.
#'
#' Time series of recorded fatalities from Snow's contribution to the Vestry Report (p. 117). Useful for studying the effect of the removal of the handle on the Broad Street pump on 08 September 1854.
#'
#' @format A data frame with 43 observations and 3 variables.
#' \describe{
#'   \item{\code{date}}{calendar date}
#'   \item{\code{fatalities}}{fatal attacks}
#'   \item{\code{deaths}}{daily deaths}
#' }
#' @docType data
"time.series.snow"
