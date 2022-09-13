Voronoi Diagrams with Longitude and Latitude Data
================

## introduction

This is a work-in-progress note about how I compute Voronoi diagrams
with geographic data (data with longitude and latitude). The
elephant-in-the-room is why not use [spatial](https://rspatial.org/) or
[GIS](https://qgis.org/) tools and approaches? Right now, my preference
is to try to extend the functionality of the existing code to include
geographic data. In the long run, this may not turn out to be the best
strategy. If any of the below seems wrong (be it obvious or subtle,
technical or conceptual), feel free to share your insights.

## georeferencing Dodson and Tobler (1992)

Dodson and Tobler (1992) digitized John Snow’s map by applying an
arbitrary “nominal” set of Cartesian coordinates to the data. Using a
modified version of those data, I estimated the longitude and latitude
of the nominal coordinates by using the “Georeferencer” tool and the
[OpenStreetMap](https://www.openstreetmap.org) XYZ tiles in
[QGIS](https://qgis.org/).

## what are Voronoi diagrams?

The best way to understand what a Voronoi diagram is and how it works is
through an example.

Consider the coffee shops in your town. For the sake of illustration,
we’ll assume that “all else being equal” (price, quality, banter, etc.,
are all the same), the only thing that affects your choice will be a
shop’s proximity. Now, if we were to repeat this computation for
everyone in town, we’ll see that distinct neighborhoods will emerge. In
the diagram below, 13 coffee shops carve up the town’s clientele into 13
neighborhoods:

``` r
snowMap()
addVoronoi()
```

![](latlongVoronoi_files/figure-gfm/voronoi-1.png)<!-- -->

In my case, the data above are not coffee shops but water pumps in 1854
Soho (Westminster, UK). The cells identify which people should use which
pump (a residence on a boundary (cell edge) is equally close to the
pump’s in adjacent neighborhoods).

## the “brute force” computation of Voronoi diagrams

The neat thing about Voronoi algorithms is that they don’t use the
“brute force” method described above. They don’t compute the proximity
for each individual. Instead, they generate the same result using only
the locations of water pumps (or coffee houses, etc.).

As “proof”, compare the diagram above, which only leverages pump
location, with the one below, which uses the “brute force” approach.

``` r
plot(neighborhoodEuclidean(case.set = "expected"), type = "star")
```

![](latlongVoronoi_files/figure-gfm/euclidean_paths-1.png)<!-- -->

To create the “brute force” diagram, I place 20,000 regularly spaced
points across the face of the map. I then computed the (Euclidean)
distance from each of these points to each of the 13 water pumps. A line
(color-coded by pump) is then plotted between a point and its closest
water pump. What emerges is a kind of photographic negative or inverse
of the first Voronoi diagram, which only used pump location. This not
only demonstrates the equivalence of the two approaches, it also gives
us a separate, independent way to validate a Voronoi diagram. This is
what will help us assess whether my effort to compute Voronoi diagrams
with geographic data is reasonable if not “right”.

## the problem

The problem is that we can’t simply apply deldir::deldir(), a 2D
algorithm, to data with geographic coordinates. You’ll get the “wrong”
answer.

Compare the following.

``` r
snowMap(latlong = FALSE)
pmp <- cholera::pumps
vars <- c("x", "y")
cells <- cholera::voronoiPolygons(pmp[, vars], rw.data = cholera::roads[, vars],
  latlong = FALSE )
invisible(lapply(cells, polygon))
title(main = "Nominal Coordinates")
```

![](latlongVoronoi_files/figure-gfm/voronoi_nominal-1.png)<!-- -->

``` r
snowMap(latlong = TRUE)
pmp <- cholera::pumps
vars <- c("lon", "lat")
cells <- cholera::voronoiPolygons(pmp[, vars], rw.data = cholera::roads[, vars],
  latlong = TRUE )
invisible(lapply(cells, polygon))
title(main = "Geographic Coordinates")
```

![](latlongVoronoi_files/figure-gfm/voronoi_naive-1.png)<!-- -->

Code-wise, the only difference between the two is that the former uses
the nominal coordinates while the latter uses geographic ones. Visually,
the difference lies with the Voronoi diagrams. You can see this, if we
remove them:

``` r
snowMap()
snowMap(latlong = TRUE)
```

<img src="latlongVoronoi_files/figure-gfm/latlong-1.png" width="50%" /><img src="latlongVoronoi_files/figure-gfm/latlong-2.png" width="50%" />

Other than the aspect ratio used to create the graphs (the nominal plot
uses asp = 1, the geographic plot uses asp = 1.65), the shape of road
network and the relative locations of the water pumps essentially look
the same. And because the location of pumps is the only data used by the
Voronoi algorithm, you might expect to get similar rather than different
Voronoi diagrams.

These facts raise two questions. First, why then are the two diagrams
different? Second, does that difference matter? The answer to the first
question is that is essentially a numerical issue. The answer to the
second question is that the naive application of the algorithm to
geographic data leads to what we might call classification errors.

## classification error

The diagrams are not just different. One is “wrong”. To show this, I
leverage the fact that there is a separate, independent way from the
algorithm to compute a Voronoi diagram: the “brute force” method
described above. By doing so, it’s pretty straightforward to show that
the diagram computed using geographic coordinate mis-classifies the
data.

The tasks. First, directly apply the Voronoi diagram algorithm to
geographic data. Second, separately compute the geodesic distance from
each observed fatality to its nearest pump.

#### some variables and parameters

``` r
vars <- c("lon", "lat")
asp <- 1.65
cases <- cholera::fatalities.address
rng <- mapRange(latlong = TRUE)
snow.colors <- snowColors(vestry = FALSE)
```

To compute and visualize the nearest pumps, I don’t directly use the
geographic coordinates. Instead, I compute the actually physical
distance between the coordinates along the Earth’s surface based on a 3D
model (WGS84 ellipsoid model). This is known as the geodesic or great
circle distance. I compute this using via geosphere::distGeo().

For each residence in the dataset that witnessed at least one fatality,
I compute which pump is nearest:

#### computation of geodesic distance to nearest pump

``` r
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

In the code below, I first plot the Voronoi cells using the raw
geographic coordinates. Then, using the results of the “brute force”
approach in `nearest.pump`, I draw line segments from the “address” of
each fatality to its nearest pump (lines are color-coded by pump). In
the plot, you can see the discrepancies between the cells and lines: the
cells do not correctly capture each fatality’s closest pump.

``` r
# plot roads and pumps
plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp)
addRoads(vars)
points(pumps[, vars], pch = 17, col = snow.colors, cex = 1)

# compute Voronoi cells by directly applying the algorithm to geographic data
cells <- cholera::voronoiPolygons(cholera::pumps[, vars],
  rw.data = cholera::roads[, vars], latlong = TRUE)

# plot Voronoi cells
invisible(lapply(cells, polygon))

# plot color-coded line segment from fatality to pump
invisible(lapply(nearest.pump$case, function(x) {
  ego <- cases[cases$anchor == x, vars]
  p <- nearest.pump[nearest.pump$case == x, "pump"]
  alter <- pumps[pumps$id == p, vars]
  segments(ego$lon, ego$lat, alter$lon, alter$lat,
    col = snow.colors[paste0("p", p)])
}))

# plot color-coded fatalities
invisible(lapply(nearest.pump$case, function(x) {
  ego <- cases[cases$anchor == x, vars]
  p <- nearest.pump[nearest.pump$case == x, "pump"]
  points(ego, pch = 16, col = snow.colors[paste0("p", p)], cex = 2/3)
}))
```

![](latlongVoronoi_files/figure-gfm/euclidean_paths_latlong_naive-1.png)<!-- -->

## numerical expectations

Why does the direct application of the algorithm to geographic data
leads to the mis-classification? The answer is essentially numerical. It
stems from the fact that even though the Earth is round, the sees the
Earth as flat. The algorithm expects a 2D Cartesian world where the only
difference between the two axes is that they are perpendicular or
orthogonal to one another. To be specific, the algorithm expects that
the distance covered by a one unit move North or South (a unit change in
latitude) will equal the distance covered by a one unit move East or
West (a unit change in longitude). Due to the Earth’s geometry, this is
not the case. As you move North or South, away the equator and toward
the poles, the distance covered as you move one unit East or West
(longitude) actually decreases. You can see this if you compare the
Equator with the Tropic of Cancer: even though all three cover the same
number of degrees of longitude, the distance along former is greater
than that along the latter.

## a solution

In the graph below, I plot my solution. Here you can see the concordance
between he algorithm-generated Voronoi diagram and the “brute
force”-generated line segments.

``` r
plot(cases[, vars], xlim = rng$x, ylim = rng$y, pch = NA, asp = asp)
addRoads(vars)
points(pumps[, vars], pch = 17, col = snow.colors, cex = 1)

cells <- cholera::latlongVoronoiC()
invisible(lapply(cells, function(dat) polygon(dat[, vars])))

invisible(lapply(nearest.pump$case, function(x) {
  ego <- cases[cases$anchor == x, vars]
  p <- nearest.pump[nearest.pump$case == x, "pump"]
  alter <- pumps[pumps$id == p, vars]
  segments(ego$lon, ego$lat, alter$lon, alter$lat, lwd = 0.5,
    col = snow.colors[paste0("p", p)])
}))

invisible(lapply(nearest.pump$case, function(x) {
  ego <- cases[cases$anchor == x, vars]
  p <- nearest.pump[nearest.pump$case == x, "pump"]
  points(ego, pch = 16, col = snow.colors[paste0("p", p)], cex = 0.5)
}))
```

![](latlongVoronoi_files/figure-gfm/euclidean_paths_latlong-1.png)<!-- -->

This solution employs a familiar strategy. Transform the data so that
you use the algorithm. Then translate the results back to original units
(longitude and latitude). The devil (and possible objections) lies in
the details.

First, to capture the effect of the Earth’s shape, I compute the
geodesic distance between an arbitrary point of reference (a “corner of
the map”) and the location of the 13 water pumps. Second, I break down
these distances into two components: meters-North and meters-East of the
reference point. These components, which are our analogs of latitude and
longitude, serve as the new set of coordinates.

The virtue of these new coordinates is that they obey the algorithm’s
expectation about distance (the distance covered by a unit move North
equals that covered by a unit move East). More importantly, when I use
them to compute the Voronoi diagrams, I get “right” results we see in
the graph above.

#### four corners

Origin is bottom left; graph in quadrant I

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

## translating back to longitude and latitude

To translate the results (the coordinates of Voronoi cells) from
meters-East and meters-North back to longitude and latitude, I use data
simulation to uncover the relationship between geodesic distance and
units of longitude and latitude.

While the measure of geodesic distance I use, geosphere::distGeo(), use
an ellipsoid model of the Earth (WGS 84), the relationship between
meters-North and latitude is perfectly linear for Soho, Westminster
(UK). Nevertheless, I play it conservatively and use a fitted loess
function to translate meters-North to latitude. The details are in the
meterLatitude():

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
OLS line might be adequate, I fit a separate loess function for each
observed meters-North value. The details are in the meterLatLong():

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

Finally, as a sanity check, I replicate the “right” and “wrong” diagrams
using [‘terra’](https://cran.r-project.org/package=terra)

``` r
dat <- cholera::pumps[, c("lon", "lat")]

sv.data <- terra::vect(dat, crs = "+proj=longlat")

pt1 <- "+proj=lcc +lat_1=51.510 +lat_2=51.516"
pt2 <- "+lat_0=51.513 +lon_0=-0.1367 +units=m"
proj <- paste(pt1, pt2)
sv.proj <- project(sv.data, proj)

v1 <- terra::voronoi(sv.proj)
v2 <- terra::voronoi(sv.data)
out1 <- terra::project(v1, "+proj=longlat")
out2 <- terra::project(v2, "+proj=longlat")
```

``` r
plot(out1, xlim = range(cholera::roads$lon), ylim = range(cholera::roads$lat))
addRoads(c("lon", "lat"))
points(cholera::pumps[, c("lon", "lat")])
text(cholera::pumps[, c("lon", "lat")], labels = paste0("p", 1:13), pos = 1)
```

![](latlongVoronoi_files/figure-gfm/terra1-1.png)<!-- -->

``` r
plot(out2, xlim = range(cholera::roads$lon), ylim = range(cholera::roads$lat))
addRoads(c("lon", "lat"))
points(cholera::pumps[, c("lon", "lat")])
text(cholera::pumps[, c("lon", "lat")], labels = paste0("p", 1:13), pos = 1)
```

![](latlongVoronoi_files/figure-gfm/terra2-1.png)<!-- -->