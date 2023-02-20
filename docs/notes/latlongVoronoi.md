computing Voronoi diagrams with geographic data
================

## introduction

This is a work-in-progress note about how I compute Voronoi diagrams
with geographic data (data with longitude and latitude). The problem is
that you can’t directly apply the standard Voronoi algorithm, e.g.,
deldir::deldir(), to such data. Doing so will give you the “wrong”
answer.

Part of the reason why is that even though the Earth is a 3D object and
its surface is curved, the standard algorithm expects the “world” to be
2D flat. While there are [spatial](https://rspatial.org/) and GIS-style
tools and methods that one can use to address this, I’m currently
hesitant to adopt them. While doing so may be the wiser long term
strategy, right now I’m simply going to try to extend the functionality
of [‘cholera’](https://github.com/lindbrook/cholera) to include
georeferenced versions of the package’s data. My strategy for addressing
this problem is a familiar one: transform the data so that you *can* use
the algorithm; translate the results back to the original form
(longitude and latitude).

If any of the below seems wrong (be it obvious or subtle, technical or
conceptual), feel free to share your comments and insights.

## what are Voronoi diagrams?

The best way to understand what a Voronoi diagram is and how it works is
through an example. Consider the coffee shops in your town. Let’s assume
that “all else being equal” (price, quality, banter, etc.) and that the
only thing that affects your choice is proximity. If you were an
algorithm, you’d compute the distance to each coffee shop and adopt the
closest one as your preferred choice. Now, if everyone in town were to
do the same, distinct “neighborhoods”, based on people’s proximity to a
shop, will emerge. In this way, the coffee shops would carve up your
town.

Here’s a hypothetical example of a town where 13 coffee shops (blue
triangles) create 13 “neighborhoods” (polygons or cells):

<img src="latlongVoronoi_files/figure-gfm/voronoi-1.png" style="display: block; margin: auto;" />

If you lived within the boundaries of a given cell then that cell’s
coffee shop will be the one that’s closest to you If you lived on a
boundary between two neighborhoods (a cell edge) then you’d happily go
to either of those neighborhoods’ shop because they’d be equally distant
to your home.

## the data

The data in Figure 1 actually come from a digitization of John Snow’s
map of the 1854 London cholera outbreak done by Dodson and Tobler
(1992). So instead of coffee houses, we’re looking at water pumps, and
instead of a hypothetical town, we’re looking at the streets of Soho.

Dodson and Tobler applied a set of Cartesian coordinates to the data.
I’ll refer to data that use these coordinate as the *nominal* data. I
estimated longitude and latitude (i.e., georeferenced) of these data by
using [QGIS](https://qgis.org/) and its `Georeferencer` tool and its
[OpenStreetMap](https://www.openstreetmap.org) `XYZ` tiles. I’ll refer
to data that use longitude and latitude as *geographic* data.

## the problem

The problem I’m concerned with is that when you try to compute a Voronoi
diagram by *directly* applying the standard algorithm, e.g.,
deldir::deldir(), to *geographic* data you get the “wrong” result:

<img src="latlongVoronoi_files/figure-gfm/wrong-1.png" style="display: block; margin: auto;" />

While there’s nothing obviously “wrong” with the diagram, I’m going to
argue that there are two problems with it. First, it doesn’t look like
diagram fitted to the nominal data in Figure 1. Second, it actually
places some people in the wrong neighborhood, which is something that,
by definition, shouldn’t happen.

## what should the Voronoi diagram should look like?

The Voronoi diagram should actually look similar to the diagram in
Figure 1 even though they’re fitted to data with different coordinates,
nominal and geographic. To understand why, we need to dig a little
deeper into how Voronoi diagrams work. Above, I actually described the
intuition behind them: find the closest pump for everyone in town and
distinct neighborhoods will emerge. This *pumps-and-people* approach is
the “brute force” way to uncover neighborhoods. However, the
implementation behind the standard algorithm actually uses a
*pumps-only* approach to the problem. This is one of the really neat
things about the algorithm: you only need the location of water pumps to
compute the Voronoi diagram (it’s also makes algorithm efficient).

Because of this, the geographic Voronoi diagram *should* look the
nominal one. After all, as we can see in Figures 4 & 5, which exclude
the Voronoi diagrams, the relative positions of the pumps for the
nominal and geographic data are quite similar:

``` r
cases <- cholera::fatalities.address
snow.colors <- snowColors(vestry = FALSE)

rng <- mapRange()
asp <- 1
vars <- c("x", "y")
plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp)
addRoads()
points(pumps[, vars], pch = 17, col = snow.colors, cex = 1)
text(pumps[, vars], pos = 1, labels = pumps$id)
points(cases[, vars], pch = 16, col = "gray", cex = 0.5)
title(main = "Figure 4 Nominal")

vars <- c("lon", "lat")
rng <- mapRange(latlong = TRUE)
asp <- 1.6
plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp)
addRoads(latlong = TRUE)
points(pumps[, vars], pch = 17, col = snow.colors, cex = 1)
text(pumps[, vars], pos = 1, labels = pumps$id)
points(cases[, vars], pch = 16, col = "gray", cex = 0.5)
title(main = "Figure 5 Geographic")
```

<img src="latlongVoronoi_files/figure-gfm/nominal_geographic-1.png" width="50%" /><img src="latlongVoronoi_files/figure-gfm/nominal_geographic-2.png" width="50%" />

As a result, the graph should look like this:

<img src="latlongVoronoi_files/figure-gfm/adjusted-1.png" style="display: block; margin: auto;" />

As we’ll see in the next section, that it looks like Figure 1 is no
coincidence.

### “brute force” pumps-and-people method behind the intuition

While it may not be computationally efficient, the value of the “brute
force” *pumps-and-people* approach goes beyond its ability to illustrate
the underlying intuition. For me, its value lies with the fact that it
provides a separate, independent way to compute and validate a Voronoi
diagram

As “proof” of the equivalence of the *pumps-and-people* (intuition) and
the *pumps-only* (implementation) methods, compare the *pumps-only*
diagram in Figure 1 with the illustration of the “brute force” method in
Figure 6:

<img src="latlongVoronoi_files/figure-gfm/euclidean_paths-1.png" style="display: block; margin: auto;" />

I created this graph by placing 20,000 regularly spaced points across
the face of the map. Next, I computed the distance from each point to
the 13 water pumps. Then, I drew a line from each point to its closest
pump (color-coded by pump). What emerges is evidence of equivalence in
the form of a kind of photographic negative or inverse of the pump-only
diagram.

With this tool in hand, I’ll show that the “Unadjusted Geographic” graph
in Figure 2 is “wrong” because it places some people in the wrong
neighborhood, which is something that shouldn’t happen by definition.

## classification error

To show that the diagram in Figure 2 mis-classifies some people, we need
to implement the “brute force” approach for geographic data. To do that
we need a way compute to compute distances between pumps and people
using geographic coordinates. I do so by using the geosphere::distGeo()
function to compute the the geodesic or great circle distance. This is
the distance between geographic coordinates along the curved surface of
the Earth as determined by the WGS84 ellipsoid model.

The key assumption here is that these geodesic distances represent the
“truth on the ground”. As such, they serve as the point of reference to
compare diagrams in Figures 2 and 3. Figures 7 and 8 re-plot those
diagrams and add the independently computed geodesic distances as
color-coded line segments that connect a person to their nearest pump.
Note that in contrast to the “brute force” plot in Figure 6, I’m only
using the 321 unique locations in the data that had at least one
fatality:

``` r
# "brute force" computation of geodesic distance to nearest pump

cases <- cholera::fatalities.address

nearest.pump <- do.call(rbind, lapply(cases$anchor, function(x) {
  p1 <- cases[cases$anchor == x, vars]
  d <- vapply(pumps$id, function(p) {
    p2 <- pumps[pumps$id == p, vars]
    geosphere::distGeo(p1, p2)
  }, numeric(1L))
  nearest <- which.min(d)
  data.frame(case = x, pump = pumps$id[pumps$id == nearest],
    meters = d[nearest])
}))
```

<img src="latlongVoronoi_files/figure-gfm/unadjusted_classification-1.png" style="display: block; margin: auto;" />

<img src="latlongVoronoi_files/figure-gfm/adjusted_classification-1.png" style="display: block; margin: auto;" />

In the “Unadjusted Geographic” graph (Figure 8), you can see
discrepancies between the cells and a person’s closest pump, which by
definition shouidn’t occur. In the “Adjusted Geographic” graph (Figure
9) there are no discrepancies.

## a solution

How does this adjustment in Figure 8 work? I use a familiar strategy: I
transform the data so that I *can* use the standard algorithm; I then
translate the results back to their original form (longitude and
latitude).

First, I transform the geographic coordinates so that they represent
relative rather than absolute positions. I do so by computing the
geodesic distance from an arbitrary point of reference (a “corner of the
map”) to the 13 water pumps. Second, I break down those distances into
its horizontal and vertical components, *meters-North* and *meters-East*
of the reference point. These components, the analogs of latitude and
longitude, serve as our new set of coordinates. The virtues of these new
coordinates is that they are Cartesian coordinates, which the standard
algorithm “expects”, and that they allow us to apply the standard
algorithm and to get the “right” results (Figure 8). Third, I translate
the results, which are the coordinates of the cell vertices, back to
longitude and latitude. To do that, I use data simulation to uncover the
relationship between geodesic distance and longitude, and the
relationship between geodesic distance and latitude. While both
relationships, for Soho at least, could statistically speaking be
summarized with a linear model (OLS), I instead fit both to separate
loess functions. I then use those functions to translate the data back
to longitude and latitude.

## spatial data approach via ‘terra’

Finally, as a sanity check and as additional piece of evidence that I
may be on the right track, I replicate the “Unadjusted” and “Adjusted”
diagrams using the [spatial](https://rspatial.org/) approach employed in
the [‘terra’](https://cran.r-project.org/package=terra) package.

``` r
dat <- cholera::pumps[, c("lon", "lat")]
sv.data <- terra::vect(dat, crs = "+proj=longlat")
v1 <- terra::voronoi(sv.data)

pt1 <- "+proj=lcc +lat_1=51.510 +lat_2=51.516"
pt2 <- "+lat_0=51.513 +lon_0=-0.1367 +units=m"
proj <- paste(pt1, pt2)
sv.proj <- terra::project(sv.data, proj)
v2 <- terra::voronoi(sv.proj)

out1 <- terra::project(v1, "+proj=longlat")
out2 <- terra::project(v2, "+proj=longlat")

plot(out1, xlim = range(cholera::roads$lon), ylim = range(cholera::roads$lat))
addRoads(latlong = TRUE)
points(cholera::pumps[, c("lon", "lat")])
text(cholera::pumps[, c("lon", "lat")], labels = 1:13, pos = 1)
title(main = "Figure 10 Undjusted Geographic")

plot(out2, xlim = range(cholera::roads$lon), ylim = range(cholera::roads$lat))
addRoads(latlong = TRUE)
points(cholera::pumps[, c("lon", "lat")])
text(cholera::pumps[, c("lon", "lat")], labels = 1:13, pos = 1)
title(main = "Figure 11 Adjusted Geographic")
```

<div class="figure">

<img src="latlongVoronoi_files/figure-gfm/terra_data-1.png" alt="Figure 7 Nominal Analog with 'terra' package" width="50%" /><img src="latlongVoronoi_files/figure-gfm/terra_data-2.png" alt="Figure 7 Nominal Analog with 'terra' package" width="50%" />
<p class="caption">
Figure 7 Nominal Analog with ‘terra’ package
</p>

</div>

## appendix: solution

The following walks through the code for the working solution, which can
be found in
[latlongVoronoi()](https://github.com/lindbrook/cholera/blob/master/R/latlongVoronoi.R)

#### four corners

Origin is bottom left; graph is in quadrant I

``` r
origin <- data.frame(lon = min(cholera::roads$lon),
                     lat = min(cholera::roads$lat))
topleft <- data.frame(lon = min(cholera::roads$lon),
                      lat = max(cholera::roads$lat))
bottomright <- data.frame(lon = max(cholera::roads$lon),
                          lat = min(cholera::roads$lat))
topright <- data.frame(lon = max(cholera::roads$lon),
                       lat = max(cholera::roads$lat))
```

#### break down geodesic distance into horizontal and vertical components

Compute geodesic distance from origin to points and decompose

``` r
pump.data <- cholera::pumps

pump.meters <- do.call(rbind, lapply(pump.data$id, function(p) {
  pmp <- pump.data[pump.data$id == p, c("lon", "lat")]
  x.proj <- c(pmp$lon, origin$lat)
  y.proj <- c(origin$lon, pmp$lat)
  m.lon <- geosphere::distGeo(y.proj, pmp)
  m.lat <- geosphere::distGeo(x.proj, pmp)
  data.frame(pump = p, x = m.lon, y = m.lat)
}))
```

#### bounding box of Voronoi diagram

``` r
height <- geosphere::distGeo(origin, topleft)
width <- geosphere::distGeo(origin, bottomright)
bounding.box <- c(0, width, 0, height)
```

#### cell coordinates

Apply deldir::deldir() and extract coordinates of cells, for use with
polygon().

``` r
cells <- voronoiPolygons(pump.meters[, c("x", "y")], rw = bounding.box)
```

#### reshape and reformat into data frame

``` r
cells.df <- do.call(rbind, cells)
cells.lat <- sort(unique(cells.df$y), decreasing = TRUE) # unique latitudes
tmp <- row.names(cells.df)
ids <- do.call(rbind, strsplit(tmp, "[.]"))
cells.df$cell <- as.numeric(ids[, 2])
cells.df$vertex <- as.numeric(ids[, 3])
row.names(cells.df) <- NULL
```

#### translating back to longitude and latitude

To translate the results (the coordinates of Voronoi cells) from
meters-East and meters-North back to longitude and latitude, I use data
simulation to uncover the relationship between geodesic distance and
units of longitude and latitude.

While the measure of geodesic distance I use, geosphere::distGeo(), use
an ellipsoid model of the Earth (WGS 84), the relationship between
meters-North and latitude is perfectly linear for Soho, Westminster
(UK). Nevertheless, I play it conservatively and use a fitted loess
function to translate meters-North to latitude. The details are in the
[meterLatitude()](https://github.com/lindbrook/cholera/blob/master/R/latlongAuxilliaryFunctions.R):

``` r
meterLatitude <- function(cells.df, origin, topleft, delta = 0.000025) {
  lat <- seq(origin$lat, topleft$lat, delta)

  meters.north <- vapply(lat, function(y) {
    geosphere::distGeo(origin, cbind(origin$lon, y))
  }, numeric(1L))

  loess.lat <- stats::loess(lat ~ meters.north,
    control = stats::loess.control(surface = "direct"))

  y.unique <- sort(unique(cells.df$y))

  est.lat <- vapply(y.unique, function(m) {
    stats::predict(loess.lat, newdata = data.frame(meters.north = m))
  }, numeric(1L))

  data.frame(m = y.unique, lat = est.lat)
}
```

East-West (horizontal) distance is a function of how far North or South
(latitude) you are. This means that the relationship between meters-East
and longitude will be nonlinear. While analysis indicates that a fitted
OLS line might be adequate, I fit separate loess functions for each
observed meters-North value. The details are in the
[meterLatLong()](https://github.com/lindbrook/cholera/blob/master/R/latlongAuxilliaryFunctions.R):

``` r
meterLatLong <- function(cells.df, origin, topleft, bottomright,
  delta = 0.000025) {

  est.lat <- meterLatitude(cells.df, origin, topleft)

  # uniformly spaced points along x-axis (longitude)
  lon <- seq(origin$lon, bottomright$lon, delta)

  # a set of horizontal distances (East-West) for each estimated latitude
  meters.east <- lapply(est.lat$lat, function(y) {
    y.axis.origin <- cbind(origin$lon, y)
    vapply(lon, function(x) {
      geosphere::distGeo(y.axis.origin, cbind(x, y))
    }, numeric(1L))
  })

  loess.lon <- lapply(meters.east, function(m) {
    dat <- data.frame(lon = lon, m)
    stats::loess(lon ~ m, data = dat,
      control = stats::loess.control(surface = "direct"))
  })

  y.unique <- sort(unique(cells.df$y))

  # estimate longitudes, append estimated latitudes
  est.lonlat <- do.call(rbind, lapply(seq_along(y.unique), function(i) {
    dat <- cells.df[cells.df$y == y.unique[i], ]
    loess.fit <- loess.lon[[i]]
    dat$lon <- vapply(dat$x, function(x) {
      stats::predict(loess.fit, newdata = data.frame(m = x))
    }, numeric(1L))
    dat$lat <- est.lat[est.lat$m == y.unique[i], "lat"]
    dat
  }))

  est.lonlat[order(est.lonlat$cell, est.lonlat$vertex), ]
}
```

The code below shows the data that translates the coordinates for the
cell for pump 1 from meters-East and meters-North to longitude and
latitude.

``` r
est.lonlat <- meterLatLong(cells.df, origin, topleft, bottomright)
longlat <- split(est.lonlat, est.lonlat$cell)
```

``` r
longlat[[1]]
```

    ##          x        y cell vertex        lon      lat
    ## 1 343.3471 826.3265    1      1 -0.1388176 51.51670
    ## 2   0.0000 826.3265    1      2 -0.1437639 51.51670
    ## 3   0.0000 703.0100    1      3 -0.1437639 51.51559
    ## 4 373.7472 707.5590    1      4 -0.1383798 51.51563
