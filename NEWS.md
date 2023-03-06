### cholera 0.8.0.9001

- add latlongStreetNameLocator().


### cholera 0.8.0.9000

- enable snowMap(add.landmarks = TRUE).
- add/set addLandmarks(latlong = FALSE).
- add/set addLandmarks(text.col = "black").
- add Golden Square and Soho Square to addLandmarks(latlong = TRUE).
- add point only for St Luke's Church in addLandmarks(latlong = FALSE).
- add point and pos = 4 label for St Luke's Church in 
  addLandmarks(latlong = TRUE).
- enable addLandmarks(highlight.perimeter = TRUE, latlong = TRUE).
- add Adam and Eve Court and Falconberg Court and Mews to 
  addLandmarks(latlong = TRUE).


### cholera 0.8.0

#### New Functions

- caseDistance().
- pumpFatalities().


#### New Data

- latlong.ortho.addr, latlong.ortho.pump and latlong.ortho.pump.vestry.
- frame.data.


#### Fixes

- fix addVoronoi(color).
- fix nearestPump(metric = "euclidean", vestry = TRUE).
- fix/change unitMeter(distance.unit = "native").
- use aes_string() in profile2D().


#### Data Changes

- use Poland Street as street address for St James Workhouse.
- re-compute ortho.proj with amended unstackFatalities().
- roadSegmentFix() places cases 440 and 145 on road segment "259-1".


#### Function Changes

- add exception for observed v. expected in neighborhoodData() via embedSites().
- add/set addCase(pch = 1, cex = 1, point.lwd = 2).
- add addCase(case %in% c("all", "anchor"), col = col).
- add/set addFrame(col = "black").
- add/set latlong = FALSE in addFrame() and addRoads().
- add/set plot.euclidean(add.title = TRUE).
- add/set plot.walking_path(stacked = TRUE).
- add/set segmentLocator(cex.text = 0.67).
- add/set streetHighlight(col = "red", lwd = 3).
- add/set walkingPath(null.origin.landmark = FALSE).
- add/use duplicateNode() in neighborhoodData() via embedSites().
- amend addVoronoi().
- amend error/exception handling in walkingPath().
- amend unstackFatalities() and use roadSegmentFix().
- fix/amend milePosts() for distance and time.


#### Longitude and Latitude Prototypes

- add/set snowMap(latlong = FALSE).
- add/set addPump(latlong = FALSE).
- add/ser roadSegments(latlong = FALSE).
- add/set voronoiPolygons(latlong = FALSE).
- latlongNeighborhoodVoronoi().
- latlongNeighborhoodWalking().
- latlongWalkingPath().


#### Documentation

- note on "computing Voronoi diagrams with geographic data".


#### Archived/Deprecated Functions

- roadHighlight().


### cholera 0.7.9

#### Note

- an interim release to address code changes in 'deldir' v1.0-2.

#### New Function

- add streetNames().

#### Data Change

- amend oxford.weather data to include entire data set.

#### Fixes

- amend code for 'deldir' v1.0-2: addDelaunay().
- fix addSnow().
- fix pumpTokens() for plot.walking(type = "roads").

#### Function Changes

- add/set addPump(cex = 1).
- add/set segmentHighlight(col = "red").
- add/set plot.oxfordWeather(month = "september").
- amend plot.oxfordWeather(statistic = "rain").
- change addDelauny() to addDelaunay().
- change type = "road" to "roads" in plot.walking().
- "gray" out unobserved and unselected pumps in pumpTokens().


### cholera 0.7.5

#### New Data

- oxford.weather

#### New Functions

- isoLines()
- isoVertices()
- oxfordWeather()
- povertyLondon()
- segmentHighlight()
- winterTemperatures()

#### Function Changes

- add addCase(case = NULL).
- add and set neighborhoodVoronoi(case.location = "address").
- add and set neighborhoodVoronoi(pump.location = "nominal").
- add subtitles for selected pumps in plot.euclidean().

#### Fixes

- fix unneeded warnings in addEuclideanPath().
- fix typo, from Silver Street to Cross Street, for meter benchmark in roads
  vignette.

#### Vignettes

- add discussion of Euclidean and Voronoi "expected" neighborhoods.


### cholera 0.7.0

#### New Feature

- support for parallel computation on Windows.

#### Fixes

- fix computation of core in neighborhoodWalking().
- fix nearestPump(metric = "euclidean", case.set = ("observed", "expected")).

#### Function Changes

- rename and amend simWalkingDistance() to simulateWalkingDistance().
- deprecate case.set argument in addNeighborhoodCases().
- add nearestPump(metric = "walking", case.set = "expected").
- amend expectedCount() for summary.walking().
- add unstackAuxiliaryFunctions.R

#### Vignettes

- add Parallelization vignette.


### cholera 0.6.5

#### Fixes

- fix plot.walking_path() timeposts.
- fix plot.walking_path(observed = FALSE).
- fix St James Workhouse for walkingPath(type = "cases").
- fix St James Workhouse for euclideanPath(type = "cases").
- fix city squares for plot.euclidean_path().
- fix computed time in nearestPump() and amend sim.walking.distance.

#### New Data

- add sim.walking.distance.

#### Function Changes

- add nearestPump(metric = "euclidean").
- add distance.unit argument to addWhitehead().
- add pch and point.size arguments to addNeighborhoodCases().
- add pos argument to addCase().
- add output argument to voronoiPolygons().
- add summary.euclidean(), summary.voronoi() and summary.walking().
- add multi.core argument to simWalkingDistance().
- add case.location to addEuclideanPath().
- add 'case' argument to pumpCase().
- amend travelingSalesman() argument in addSnow().
- amend print.euclidean(), print.voronoi() and print.walking().
- amend argument 'case' to 'data' in addKernelDensity().
- amend plot.walking() titles.
- deprecate pearlString() in favor of travelingSalesman().
- deprecate statistic argument in neighborhoodVoronoi().
- enable col argument in addRoads().
- enable 'address' and 'nominal' cases in plot.voronoi().
- exclude landmarks from case.set - "expected".
- rename deldirVertices() back to voronoiPolygons().

#### Vignettes

- rename "deldirPolygons()" to "voronoiPolygons()".
- remove Pump Neighborhoods vignette.

#### DESCRIPTION

- remove 'scales' from Imports.


### cholera 0.6.0

#### Fixes

- fix title in euclideanPath(type = "case-pump").
- fix destination label for walkingPath(destination = NULL).

#### Data Changes

- add Earl of Aberdeen residence (Argyll House).
- nominal and orthogonal coordinates for landmarks.

#### Function Changes

- addNeighborhood() -> addNeighborhoodWalking()

#### Function Changes - new arguments

- addSnow(type = "perimeter", line.width = 2)
- neighborhoodData(embed = TRUE, embed.landmarks = TRUE)
- neighborhoodEuclidean(case.set = "expected")
- plot.voronoi(voronoi.cells = TRUE, delauny.triangles = FALSE)
- snowMap(...)
- streetNameLocator(add.subtitle = TRUE, token = id)
- streetNumberLocator(add.subtitle = TRUE, token = id)

#### Function Changes - polygon.method argument

- addNeighborhoodEuclidean(polygon.method = "traveling.salesman")
- plot.euclidean(polygon.method = "traveling.salesman")

- addNeighborhoodWalking(polygon.method = "pearl.string")
- plot.walking(polygon.method = "pearl.string")

#### Function Change - landmarks as origin and/or destination (treated as cases)

- euclideanPath()
- walkingPath()
- find nearest case or landmark, given pump (i.e., reverse lookup)

#### Function Changes - case.location argument: "address" or "nominal"

- addVoronoi(case.location = "nominal")
- euclideanPath(case.location = "nominal")
- neighborhoodEuclidean(case.location = "nominal")
- addNeighborhoodEuclidean(case.location = "nominal")

#### New Functions

- addCase()
- addDelauny()
- addNeighborhoodCases()
- deldirVertices()
- orthogonalProjection()
- profile2D()
- profile3D()
- streetHighlight()

#### New Exported Functions

- fixFatalities()
- landmarkData()

#### New S3 Function

- pearsonResiduals()
- plot.neighborhood_data()

#### New Vignette

- "deldirVertices(): Tiles, Triangles and Polygons"

#### Deprecated Functions

- euclideanDistance()
- walkingDistance()


### cholera 0.5.1

#### Fixes

- backward compatibility (R 3.4.4) related to base::isFALSE() & bug fix.
- fix for multiple results in walkingDistance() and walkingPath().

#### Function Changes

- enable ellipses (...) in plot.time_series() (#1).
- enable ellipses and negative selection in addPump().
- consolidate addEuclideanPath(), euclideanDistance(), euclideanPath(),
  walkingDistance() and walkingPath()

#### New Functions

- addBorder()
- addRoads()
- mapRange()


### cholera 0.5.0

#### Data Changes

- regular.cases and sim.ortho.proj:
  increase number of observations from 5K to 20K.

#### Function Changes

- "alpha.level" argument to control path transparency
    addEuclideanPath() and addWalkingPath()

- distance and time based "mileposts"
    addEuclideanPath() and addWalkingPath().
    plot.euclidean_path() and plot.walking_path().
    addMilePosts().

- "pump.subset" and "pump.select" arguments
    addCase(), addKernelDensity(), addMilePosts(), addNeighborhood(),
    neighborhoodEuclidean(), neighborhoodWalking()

- "walking.speed" argument added to:
    addMilePosts(), nearestPump(),
    addEuclideanPath(), euclideanDistance(), euclideanPath(),
    addWalkingPath(), walkingDistance(), walkingPath()

- euclideanDistance() no longer S3.
    generic S3 functionality moved to euclideanPath().

- multiCore() moved to multiCore.R.

- neighborhoodVoronoi()
    plot.voronoi() adds "euclidean.paths" argument for star graph.

- neighborhoodWalking()
    "area.polygons" related functions for plot_walking() moved to
    pearlString.R.

- simulateFatalities():
  default is now 20K observations.
  use proximate in addition to orthogonal distances to find "addresses".

- snowMap() new arguments:
  "add.cases", "add.pumps", "add.roads".

- unitMeter() default unit of measurement is now "meter".

- walkingAuxillaryFunctions.R:
    location of walking related helper functions.

- walkingDistance() no longer S3.
    generic S3 functionality moved to walkingPath().

#### New Functions

- addCase()
- addEuclideanPath()
- addMilePosts()
- addNeighborhood()
- addWalkingPath()()
- distanceTime()

#### New S3 Functions

- euclideanPath()
- walkingPath()
- neighborhoodEuclidean()

#### Vignette Changes

- Lab Notes available online and on GitHub:
  "duplicate.missing.cases.notes"
  "pump.neighborhoods.notes"
  "unstacking.bars.notes"


### cholera 0.4.0

#### Data Changes

- ortho.proj.pump and ortho.proj.pump.vestry now include node ID.

- roads and road.segments amend street names:
    "Unknown-B" to "Tent Court" (Edmund Cooper's map).
    "Unknown-D" to "St James's Market" (https://maps.nls.uk).
    "Unknown-E" to "Market Street (II)" (https://maps.nls.uk).

#### Function Changes

- addKernelDensity()
  uses "pump.subset" and "pump.select" arguments.

- addLandmarks()
  add landmarks from Edmund Cooper's map.

- classifierAudit() can return coordinates of address.

- nearestPump() now incorporates nearestPath().

- neighborhoodWalking()
  segment and sub-segment implementation.

- pumpData()
  returns node ID.

- timeSeries()
  includes day of the week.

- walkingDistance()
  add "simulated" expected cases.

#### New Functions

- addNeighborhood()

#### New S3 Implementations

- plot.walking
  type = "area.points" and type = "area-polygons".
  type = "area-polygons" via pearlString() replaces alphahull::ashape().

- print.walking() uses expectedCount().

#### Vignette Changes

- add "Kernel Density Plot".
- update "Pump Neighborhoods" with discussion of area plots.


### cholera 0.3.0

#### Data Changes

- ortho.proj:
    reclassify case 483:
      Pulteney Court (I) ("242-1") -> Little Windmill Street ("326-2").
    reclassify cases 369, 434, 11, 53, 193:
      Poland Street ("194-1") -> St James Workhouse ("148-1").

#### Function Changes

- addSnow()
    "area", "street" and "boundary" graphical annotation.

- caseLocator()
    highlight home road segment.

- neighborhoodWalking()
    "case-set" argument: "observed", "expected" and "snow".
    updated implementation and improved performance.
    pre-computed configurations from version 0.2.1 removed.

- segmentLocator(), streetNameLocator() and streetNumberLocator()
    highlight segment or street cases.
    option to plot all cases, anchor cases or no cases.

#### New S3 Implementations

- timeSeries()
- walkingDistance()
    incorporates and deprecates walkingPath().

#### New Functions

- addIndexCase()
- nearestPath()
- nearestPump()
- nodeData()
- segmentLength()
- snowNeighborhood()
- streetLength()
- unitMeter()

#### New S3 Functions

- classifierAudit()
- euclideanDistance()


### cholera 0.2.1

- Initial CRAN release.