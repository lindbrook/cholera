Lab Notes: Pearl Strings to Polygons
================
lindbrook
2018-12-15

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

These are the simulated cases within the Broad Street pump neighborhood:

![](cloud-1.png)

The convex hull of those cases:

![](hull-1.png)

To find the points on the perimeter, the candidate points for the vertices of the polygon, I select the point that *do not* have neighbors at each of the four cardinal directions. See peripheryCases().

![](perimeter-1.png)

To connect the dots, we add pearls to a string to form polygon

![](pearl_string-1.png)

two algorithms
--------------

pearlString() cycles through the candidate points and uses reverse epicycles to find the next point to add to the string of pearls. This is the default for walking neighborhoods.

travelingSalesman() uses the 'TSP' package and its implementation of repetitive nearest neighbors to compute the string of pearls. This is the defaul for euclidean neighbors.

``` r
neighborhood <- neighborhoodEuclidean(-6, case.set = "expected", vestry = TRUE)

plot(neighborhood, type = "area.polygons", method = "pearl.string")
```

<img src="pearl.strings_files/figure-markdown_github/pearl_string-1.png" style="display: block; margin: auto;" />

``` r
neighborhood <- neighborhoodEuclidean(-6, case.set = "expected", vestry = TRUE)

plot(neighborhood, type = "area.polygons", method = "traveling.salesman")
```

<img src="pearl.strings_files/figure-markdown_github/traveling-1.png" style="display: block; margin: auto;" />
