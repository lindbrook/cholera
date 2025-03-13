#' Anchor or base case of each stack of fatalities.
#'
#' Data frame that links a fatality to its stack, a stack's base case. For use with \code{\link{caseLocator}}.
#'
#' @format
#'  \describe{
#'     \item{\code{case}}{numerical case ID}
#'     \item{\code{anchor}}{numerical case ID of anchor.case}
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
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#' }
#' @seealso \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#'
#' @note fixFatalities() documents the code for these data. For details, see \code{vignette}("duplicate.missing.cases").
#' @seealso  \code{\link{caseLocator}}
#'
#' \code{\link{streetNameLocator}}
#'
#' \code{\link{streetNumberLocator}}
#' @docType data
"fatalities"

#' "Unstacked" amended cholera data with address as unit of observation.
#'
#' An "unstacked" version of the \code{fatalities} dataset. It changes the unit of observation from the case (bar) to the "address", the x-y coordinates of the case at the base of a stack, and makes the number of fatalities an attribute of the "address".
#'
#' @format A data frame with 4 variables for 321 addresses
#'  \describe{
#'     \item{\code{anchor}}{numerical case ID of address}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'     \item{\code{case.count}}{number of fatalities at address}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
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
#' An "unstacked" version of the \code{fatalities} dataset. It changes the unit of observation from the case (bar) to the "address", the x-y coordinates of the case at the base of a stack, and assigns the base case's coordinates to all cases in the stack.
#'
#' @format A data frame with 3 variable that records the position of the 578 bars on Snow's map.
#'
#'  \describe{
#'     \item{\code{case}}{numerical case ID}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
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

#' Landmark coordinates.
#'
#' @format A data frame of landmark coordinates with 20 observations and 11 variables.
#'  \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x}}{nominal x-coordinate}
#'     \item{\code{y}}{nominal y-coordinate}
#'     \item{\code{x.lab}}{label x-coordinate}
#'     \item{\code{y.lab}}{label y-coordinate}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'     \item{\code{lon.lab}}{label longitude}
#'     \item{\code{lat.lab}}{label latitude}
#'     \item{\code{name}}{landmark name}
#'  }
#' @docType data
"landmarks"

#' Centers of city squares.
#'
#' @format A data frame with 2 observations and 6 variables that records the position of landmark square labels.
#'  \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'     \item{\code{name}}{square name}
#'  }
#' @docType data
"landmark.squares"

#' Orthogonal projection of observed cases onto road network.
#'
#' @format A data frame with 5 variables that records the position of the orthogonal projection of the 578 cases onto the network of roads.
#'  \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{dist}}{distance to home road segment}
#'     \item{\code{type}}{type of distance: orthogonal to road or Euclidean to endpoint}
#'  }
#' @note \code{\link{unstackFatalities}} documents the code for these data.
#' @docType data
"ortho.proj"

#' Sample of road intersections (segment endpoints).
#'
#' @format A list with 2 variables that list randomly re-arranges unique road intersections (segment endpoints).
#'  \describe{
#'     \item{\code{one}}{endpoints with 1 intersection}
#'     \item{\code{three}}{endpoints with 3 intersections}
#'  }
#' @docType data
"rd.sample"

#' Partitioned map frame points (segment endpoints).
#'
#' @format A list of 3 vectors length 19, 19 and 18 from cholera::roads$id.
#'  \describe{
#'     \item{\code{frame.sample}}{cholera::roads$id}
#'  }
#' @docType data
"frame.sample"

#' Map frame data c("x", "y") and c("lon", "lat").
#'
#' @format A data frame with 106 observations (points) and 8 variables.
#'  \describe{
#'     \item{\code{street}}{street number}
#'     \item{\code{n}}{street street component number}
#'     \item{\code{x}}{native x-coordinate}
#'     \item{\code{y}}{native y-coordinate}
#'     \item{\code{id}}{segment numeric ID}
#'     \item{\code{name}}{street name}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @docType data
"frame.data"

#' Road "address" of simulated (i.e., "expected") cases.
#'
#' @format A data frame with 6 variables that records the "address" of 19,993 simulate cases along the network of roads.
#' \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{dist}}{Euclidean or orthogonal distance to home road segment}
#'     \item{\code{type}}{type of projection: Euclidean ("eucl") or orthogonal ("ortho")}
#'  }
#' @note \code{\link{simulateFatalities}} documents the code for these data.
#' @docType data
"sim.ortho.proj"

#' Orthogonal projection of 13 original pumps.
#'
#' @format A data frame with 6 variables that records the position of the orthogonal projection of the 13 original pumps onto the network of roads.
#'  \describe{
#'     \item{\code{pump.id}}{numeric ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{node}}{node ID}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"ortho.proj.pump"

#' Orthogonal projection of the 14 pumps from the Vestry Report.
#'
#' @format A data frame with 6 variables that records the position of the orthogonal projection of the 14 pumps onto the network of roads.
#'  \describe{
#'     \item{\code{pump.id}}{numeric ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{node}}{node ID}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"ortho.proj.pump.vestry"

#' Rectangular filter data.
#'
#' Coordinates to filter out frame shadow using sp::point.in.polygon().
#' @format A data frame with 2 variables and 4 observations.
#'  \describe{
#'     \item{\code{x}}{longitude}
#'     \item{\code{y}}{latitude}
#'  }
#' @docType data
"rectangle.filter"

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
#'   \item{\code{lon}}{longitude}
#'   \item{\code{lat}}{latitude}
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
#'   \item{\code{lon}}{longitude}
#'   \item{\code{lat}}{latitude}
#' }
#' @seealso \code{\link{pumpLocator}}
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"pumps.vestry"

#' "Expected" cases.
#'
#' The result of using sp::spsample() and sp::Polygon() to generate 19,993 regularly spaced simulated cases within the map's borders.
#'
#' @format A data frame with 2 variable that records the position of 19,993 "expected" cases fitted by sp::spsample().
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
#' @format A data frame with 658 observations and 7 variables. The data describe the straight line segments used to recreate the roads on Snow's map.
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
#' @format A data frame with 1243 observations and 6 variables. The data describe the roads on Snow's map.
#' \describe{
#'   \item{\code{street}}{street segment number, which range between 1 and 528}
#'   \item{\code{n}}{number of points in this street line segment}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#'   \item{\code{id}}{unique numeric ID}
#'   \item{\code{name}}{road name}
#'   \item{\code{lon}}{longitude}
#'   \item{\code{lat}}{latitude}
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

#' List of "simulated" fatalities grouped by walking-distance pump neighborhood.
#'
#' @format A list 4972 IDs spread over 13 vectors.
#' \describe{
#'  \item{\code{sim.pump.case}}{numerical ID}
#' }
#' @note \code{\link{neighborhoodWalking}} documents the code for these data. For details, see \code{vignette}("pump.neighborhoods").
#' @examples
#' \dontrun{
#' pumpCase(neighborhoodWalking(case.set = "expected"))
#' }
#' @docType data
"sim.pump.case"

#' Walking distance to Broad Street Pump (#7).
#'
#' @format A data frames with 5 variables.
#' \describe{
#'   \item{\code{case}}{case ID}
#'   \item{\code{pump}}{pump ID}
#'   \item{\code{pump.name}}{pump name}
#'   \item{\code{distance}}{walking distance in meters}
#'   \item{\code{time}}{walking time in seconds based on 5 km/hr walking speed}
#' }
#' @docType data
"sim.walking.distance"

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

#' Coordinates of Voronoi polygon vertices for original map.
#'
#' @format A list of 13 data frames frames with 5 variables.
#' \describe{
#'   \item{\code{vertex}}{vertex ID}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#'   \item{\code{lon}}{longitude}
#'   \item{\code{lat}}{latitude}
#' }
#' @docType data
"voronoi.polygons"

#' Coordinates of Voronoi polygon vertices for Vestry Report map.
#'
#' @format A list of 14 data frames frames with 5 variables.
#' \describe{
#'   \item{\code{vertex}}{vertex ID}
#'   \item{\code{x}}{x-coordinate}
#'   \item{\code{y}}{y-coordinate}
#'   \item{\code{lon}}{longitude}
#'   \item{\code{lat}}{latitude}
#' }
#' @docType data
"voronoi.polygons.vestry"

#' Orthogonal projection of observed address (latlong) cases onto road network.
#'
#' @format A data frame with 7 variables that records the position of the orthogonal projection of the 321 cases onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @note \code{\link{unstackFatalities}} documents the code for these data.
#' @docType data
"latlong.ortho.addr"

#' Orthogonal projection of 13 original pumps (latlong).
#'
#' @format A data frame with 7 variables that records the position of the orthogonal projection of the 13 original pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{id}}{numeric ID}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"latlong.ortho.pump"

#' Orthogonal projection of the 14 pumps from the Vestry Report (latlong).
#'
#' @format A data frame with 7 variables that records the position of the orthogonal projection of the 14 pumps onto the network of roads.
#'  \describe{
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{ortho.dist}}{orthogonal distance to home road segment}
#'     \item{\code{id}}{numeric ID}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @note \code{\link{pumpData}} documents the code for these data.
#' @docType data
"latlong.ortho.pump.vestry"

#' Road "address" of simulated (i.e., "expected") cases (latlong).
#'
#' @format A data frame with 8 variables that records the "address" of 19,993 regularly spaced simulated Cartesian/geodesic cases regularly spaced across map.
#' \describe{
#'     \item{\code{case}}{numeric case ID}
#'     \item{\code{road.segment}}{"address" road segment}
#'     \item{\code{x.proj}}{x-coordinate}
#'     \item{\code{y.proj}}{y-coordinate}
#'     \item{\code{dist}}{Euclidean or orthogonal distance to home road segment}
#'     \item{\code{type}}{type of projection: Euclidean ("eucl") or orthogonal ("ortho")}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @docType data
"latlong.sim.ortho.proj"

#' "Expected" cases (latlong).
#'
#' The result of using sp::spsample() and sp::Polygon() to generate 19,993 regularly spaced simulated Cartesian/geodesic cases within the map's borders.
#'
#' @format A data frame with 4 variables that records the position of 19,993 "expected" cases fitted by sp::spsample().
#'  \describe{
#'     \item{\code{x}}{x-coordinate}
#'     \item{\code{y}}{y-coordinate}
#'     \item{\code{lon}}{longitude}
#'     \item{\code{lat}}{latitude}
#'  }
#' @docType data
"latlong.regular.cases"

#' Meter to yard conversion factor.
#'
#' @format A vector of length one.
#'  \describe{
#'     \item{\code{meter.to.yard}}{conversion factor = 1.093613}
#'  }
#' @docType data
"meter.to.yard"
