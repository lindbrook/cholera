#' Anchor or base case of each stack of fatalities.
#'
#' Data frame that links a fatality to its stack, a stack's base case. For use with \code{\link{caseLocator}}.
#'
#' @format
#'  \describe{
#'     \item{\code{case}}{numerical case ID}
#'     \item{\code{anchor.case}}{numerical case ID of anchor.case}
#' }
#' @note \code{\link{unstackFatalities}} documents the code for these data.
#' @docType data
"anchor.case"

#' Numeric IDs of line segments that create the map's border frame.
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
#' @seealso \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' @note \code{\link{fixFatalities}} documents the code for these data. For details, see \code{vignette}("duplicate.missing.cases").
#' @seealso  \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#' @docType data
"fatalities"

#' "Unstacked" amended cholera data with address as unit of observation.
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
#' @seealso
#' \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' @note \code{\link{unstackFatalities}} documents the code for these data. For details, see \code{vignette}("unstacking.fatalities").
#' @docType data
"fatalities.address"

#' "Unstacked" amended cholera fatalities data with fatality as unit of observation.
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
#' @seealso \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' @note \code{\link{unstackFatalities}} documents the code for these data. For details, see \code{vignette}("unstacking.fatalities").
#' @docType data
"fatalities.unstacked"

#' Orthogonal projection of observed cases onto road network.
#'
#' @format A data frame with 5 variables that records the position of the orthogonal projection of the 578 cases onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{case}}{numeric case ID}
#'  }
#' @note \code{\link{unstackFatalities}} documents the code for these data.
#' @docType data
"ortho.proj"

#' Road "address" of simulated (i.e., "expected") cases.
#'
#' @format A data frame with 6 variables that records the "address" of 20007 simulate cases along the network of roads.
#' \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{dist}}{Euclidean or orthogonal distance to home road segment}
#'     \item{\code{type}}{type of projection: Euclidean ("eucl") or orthogonal ("ortho")}
#'     \item{\code{case}}{numeric case ID}
#'  }
#' @note \code{\link{simulateFatalities}} documents the code for these data.
#' @docType data
"sim.ortho.proj"

#' Orthogonal projection of 13 original pumps.
#'
#' @format A data frame with 6 variables that records the position of the orthogonal projection of the 13 original pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{node}}{node ID}
#'     \item{\code{pump.id}}{numeric ID}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"ortho.proj.pump"

#' Orthogonal projection of the 14 pumps from the Vestry Report.
#'
#' @format A data frame with 6 variables that records the position of the orthogonal projection of the 14 pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{node}}{node ID}
#'     \item{\code{pump.id}}{numeric ID}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
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

#' List of "simulated" fatalities grouped by walking-distance pump neighborhood.
#'
#' @format A list 4972 IDs spread over 13 vectors.
#' \describe{
#'  \item{\code{sim.pump.case}}{numerical ID}
#' }
#' @note \code{\link{neighborhoodWalking}} documents the code for these data. For details, see \code{vignette}("pump.neighborhoods").
#' @examples
#' # pumpCase(neighborhoodWalking(case.set = "expected"))
#' @docType data
"sim.pump.case"

#' Dodson and Tobler's pump data with street name.
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
#' @seealso \code{\link{pumpLocator}}
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"pumps"

#' Vestry report pump data.
#'
#' These data include the fourteenth pump, at Hanover Square, and the "corrected" location of the Broad Street pump that Snow includes in the second version of his map in the Vestry report.
#'
#' @format A data frame with 14 observations and 4 variables.
#' \describe{
#'   \item{\code{id}}{pump number between 1 and 14}
#'   \item{\code{street}}{nearest street}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#' }
#' @seealso \code{\link{pumpLocator}}
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"pumps.vestry"

#' "Expected" cases.
#'
#' The result of using sp::spsample() and sp::Polygon() to generate 20007 regularly spaced simulated cases within the map's borders.
#'
#' @format A data frame with 2 variable that records the position of 20007 "expected" cases fitted by sp::spsample().
#'  \describe{
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'  }
#' @note \code{\link{simulateFatalities}} documents the code for these data.
#' @docType data
"regular.cases"

#' Dodson and Tobler's street data transformed into road segments.
#'
#' This data set transforms Dodson and Tobler's street data to give each straight line segment of a "road" a unique ID.
#'
#' @format A data frame with 657 observations and 7 variables. The data describe the straight line segments used to recreate the roads on Snow's map.
#' \describe{
#'   \item{\code{street}}{numeric street ID, which range between 1 and 528}
#'   \item{\code{id}}{character segment ID}
#'   \item{\code{name}}{road name}
#'   \item{\code{x1}}{x-coordinate of first endpoint}
#'   \item{\code{y1}}{y-coordinate of first endpoint}
#'   \item{\code{x2}}{x-coordinate of second endpoint}
#'   \item{\code{y2}}{y-coordinate of second endpoint}
#' }
#' @seealso \code{\link{roads}}
#'
#'\code{vignette}("road.names")
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' \code{\link{segmentLocator}}
#' @note \code{\link{roadSegments}} documents the code for these data.
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
#' @seealso \code{\link{road.segments}}
#'
#' \code{vignette}("road.names")
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' \code{\link{segmentLocator}}
"roads"

#' Snow neighborhood fatalities.
#'
#' Numeric IDs of fatalities from Dodson and Tobler that fall within Snow's Broad Street pump neighborhood.
#'
#' @format A vector with 384 observations.
#' \describe{
#'   \item{\code{snow.neighborhood}}{numeric case ID}
#' }
#' @docType data
"snow.neighborhood"
