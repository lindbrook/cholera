Pearl Strings to Polygons
================
lindbrook
2018-11-30

Overview
--------

Two flavors of "expected" area plots: points and polygons

``` r
plot(neighborhoodWalking(case.set = "expected"), type = "area.points")
```

<img src="pearl.strings_files/figure-markdown_github/area_points-1.png" style="display: block; margin: auto;" />

``` r
plot(neighborhoodWalking(case.set = "expected"), type = "area.polygons")
```

<img src="pearl.strings_files/figure-markdown_github/area_polygons-1.png" style="display: block; margin: auto;" />

why polygons?
-------------

The virtues of vector graphics.

``` r
streetNameLocator("marshall street", zoom = TRUE)
addNeighborhoodCases(type = "expected")
```

<img src="pearl.strings_files/figure-markdown_github/marshall_points-1.png" style="display: block; margin: auto;" />

``` r
streetNameLocator("marshall street", zoom = TRUE)
addNeighborhoodWalking()
```

<img src="pearl.strings_files/figure-markdown_github/marshall_polygons-1.png" style="display: block; margin: auto;" />

computing polygons
------------------

### Broad Street simulated cases

![](cloud-1.png)

### convex hull

![](hull-1.png)

### identify points on perimeter

![](perimeter-1.png)

### connect the dots to form polygon

![](pearl_string-1.png)

two algorithms
--------------

pearlString() : cycle + reverse epicycles

travelingSalesman() : 'TSP' package based solution using repetitive nearest neighbor.
