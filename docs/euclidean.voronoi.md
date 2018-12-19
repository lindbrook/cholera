Lab Notes: Euclidean v. Voronoi neighborhoods
================
lindbrook
2018-12-19

overview
--------

"The Voronoi region of a site ***s*** is the set of points in the plane for which ***s*** is the closest site among all the sites."[1]

Given this definition, you might wonder why there is neighborhoodEuclidean() and neighborhoodVoronoi(). First, by definition, you need at least two sites to create a Voronoi diagram. In contrast, neighborhoodEuclidean() allows you to create the star plot below, which draws all the Euclidean paths from cases to the pump \#7 on Broad Street.

``` r
plot(cholera::neighborhoodEuclidean(7))
```

![](euclidean.voronoi_files/figure-markdown_github/star-1.png)

Second, neighborhoodEuclidean() allows you to add a small dose of "realism". With Voronoi tessellation, all that matters is the location of the sites (water pumps). Other data simply don't matter: the "address" or location of fatalities, buildings, roads, etc. With neighborhoodEuclidean(), when case.set = "expected", you can use the case.location argument to either "nominal" or "address". The former uses nominal coordinates of expected cases (i.e., regular.cases), and replicates the Voronoi diagram. The latter uses the street "addresses" of expected cases, which is based on the orthogonal projection from the case's nominal location to the nearest street segment. Literally, it means that a person must leave from their front door.

``` r
plot(cholera::neighborhoodEuclidean(case.set = "expected",
  case.location = "address"), type = "area.points")
cholera::addVoronoi(case.location = "address", color = "white", lwd = 2)
```

![](euclidean.voronoi_files/figure-markdown_github/address-1.png)

``` r
plot(cholera::neighborhoodEuclidean(case.set = "expected",
  case.location = "nominal"), type = "area.points")
cholera::addVoronoi(case.location = "nominal", color = "white", lwd = 2)
```

![](euclidean.voronoi_files/figure-markdown_github/nominal-1.png)

[1] Steven Fortune. 1987. "A Sweepline Algorithm for Voronoi Diagrams". *Algorithmica*. 2:153-174.
