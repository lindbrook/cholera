NEWS
================

cholera 0.6.0
=============

### fixes

-   fix title in euclideanPath(type = "case-pump").
-   fix destination label for walkingPath(destination = NULL).

### data changes

-   add Earl of Aberdeen residence (Argyll House).

### function changes

-   addNeighborhood() -&gt; addNeighborhoodWalking()

### function changes - new arguments

-   addSnow(type = "perimeter", line.width = 2)
-   neighborhoodData(embed = TRUE, embed.landmarks = TRUE)
-   neighborhoodEuclidean(case.set = "expected")
-   plot.voronoi(voronoi.cells = TRUE, delauny.triangles = FALSE)
-   snowMap(...)
-   streetNameLocator(add.subtitle = TRUE, token = id)
-   streetNumberLocator(add.subtitle = TRUE, token = id)

### function changes - polygon.method argument

-   addNeighborhoodEuclidean(polygon.method = "traveling.salesman")
-   plot.euclidean(polygon.method = "traveling.salesman")
-   addNeighborhoodWalking(polygon.method = "pearl.string")
-   plot.walking(polygon.method = "pearl.string")

### function changes - landmarks as origin and/or destination (treated as cases)

-   euclideanDistance()
-   euclideanPath()
-   walkingDistance()
-   walkingPath()

### function changes - case.location argument: "address" or "nominal"

-   addVoronoi(case.location = "nominal")
-   euclideanDistance(case.location = "nominal")
-   euclideanPath(case.location = "nominal")
-   neighborhoodEuclidean(case.location = "nominal")

### new functions

-   addCases()
-   addDelauny()
-   addNeighborhoodCases()
-   deldirPolygons()
-   profile2D()
-   profile3D()
-   streetHighlight()

### new S3 function

-   pearsonResiduals()
-   plot.neighborhood\_data()

### new vignette

-   deldirPolygons(): Tiles, Triangles and Polygon

cholera 0.5.1
=============

### fixes

-   backward compatibility (R 3.4.4) related to base::isFALSE() & bug fix.
-   fix for multiple results in walkingDistance() and walkingPath().

### function changes

-   enable ellipses (...) in plot.time\_series() (\#1).
-   enable ellipses and negative selection in addPump().
-   consolidate addEuclideanPath(), euclideanDistance(), euclideanPath(), walkingDistance() and walkingPath()

### new functions

-   addBorder()
-   addRoads()
-   mapRange()

cholera 0.5.0
=============

### data changes

-   regular.cases and sim.ortho.proj:
    -   increase number of observations from 5K to 20K.

### function changes

-   "alpha.level" argument to control path transparency
    -   addEuclideanPath() and addWalkingPath()
-   distance and time based "mileposts"
    -   addEuclideanPath() and addWalkingPath().
    -   plot.euclidean\_path() and plot.walking\_path().
    -   addMilePosts().
-   "pump.subset" and "pump.select" arguments
    -   addCases(), addKernelDensity(), addMilePosts(), addNeighborhood(),
    -   neighborhoodEuclidean(), neighborhoodWalking()
-   "walking.speed" argument added to:
    -   addMilePosts(), nearestPump(),
    -   addEuclideanPath(), euclideanDistance(), euclideanPath(),
    -   addWalkingPath(), walkingDistance(), walkingPath()
-   euclideanDistance() no longer S3.
    -   generic S3 functionality moved to euclideanPath().
-   multiCore() moved to multiCore.R.

-   neighborhoodVoronoi()
    -   plot.voronoi() adds "euclidean.paths" argument for star graph.
-   neighborhoodWalking()
    -   "area.polygons" functions for plot\_walking() moved to pearlString.R.
-   simulateFatalities():
    -   default is now 20K observations.
    -   use proximate in addition to orthogonal distances to find "addresses".
-   snowMap() new arguments:
    -   "add.cases", "add.pumps", "add.roads".
-   unitMeter() default unit of measurement is now "meter".

-   walkingAuxillaryFunctions.R:
    -   location of walking related helper functions.
-   walkingDistance() no longer S3.
    -   generic S3 functionality moved to walkingPath().

### new functions

-   addCases()
-   addEuclideanPath()
-   addMilePosts()
-   addNeighborhood()
-   addWalkingPath()()
-   distanceTime()

### new S3 function

-   euclideanPath()
-   walkingPath()
-   neighborhoodEuclidean()

### vignette changes

-   Lab Notes available online and on GitHub:
    -   "duplicate.missing.cases.notes"
    -   "pump.neighborhoods.notes"
    -   "unstacking.bars.notes"

cholera 0.4.0
=============

### data changes

-   ortho.proj.pump and ortho.proj.pump.vestry now include node ID.

-   roads and road.segments amend street names:
    -   "Unknown-B" to "Tent Court" (Edmund Cooper's map).
    -   "Unknown-D" to "St James's Market" (<https://maps.nls.uk>).
    -   "Unknown-E" to "Market Street (II)" (<https://maps.nls.uk>).

### function changes

-   addKernelDensity()
    -   uses "pump.subset" and "pump.select" arguments.
-   addLandmarks()
    -   add landmarks from Edmund Cooper's map.
-   classifierAudit() can return coordinates of address.

-   nearestPump() now incorporates nearestPath().

-   neighborhoodWalking()
    -   segment and sub-segment implementation.
-   pumpData()
    -   returns node ID.
-   timeSeries() -includes day of the week.

-   walkingDistance()
    -   add "simulated" expected cases.

### new functions

-   addNeighborhood()

### new S3 implementations

-   plot.walking
    -   type = "area.points" and type = "area-polygons".
    -   type = "area-polygons" via pearlString() replaces alphahull::ashape().
-   print.walking() uses expectedCount().

### vignette changes

-   add "Kernel Density Plot".
-   update "Pump Neighborhoods" with discussion of area plots.

cholera 0.3.0
=============

### data changes

-   ortho.proj:
    -   reclassify case 483 from segment "242-1" to "326-2".
    -   reclassify cases 369, 434, 11, 53, 193 from segment "194-1" to "148-1".
-   regular.cases:
    -   fix for case 3173 for neighborhood area plots.

### function changes

-   addSnow()
    -   "area", "street" and "boundary" graphical annotation.
-   neighborhoodWalking()
    -   improvements in functionality and performance.
-   segmentLocator(), streetNameLocator() and streetNumberLocator()
    -   highlight cases

### function changes (S3)

-   timeSeries()
-   walkingDistance()

### new functions

-   addIndexCase()
-   nearestPath()
-   nearestPump()
-   neighborhoodDistances()
-   nodeData()
-   segmentLength()
-   snowNeighborhood()
-   streetLength()
-   unitMeter()

### new functions (S3)

-   classifierAudit()
-   euclideanDistance()

cholera 0.2.1
=============

-   Initial CRAN release.
