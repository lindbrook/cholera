Lab Notes: Pump Neighborhoods
================
lindbrook
2018-07-08

Area polygons
-------------

To draw the area polygons for pump neighborhoods, I did the following.

First, using sp::spsample and sp::Polygon, I place approximately 20K regularly-spaced points across the face of the map.

``` r
sp::spsample(sp::Polygon(map.frame[, c("x", "y")]), n = 20000, type = "regular")
```

For each simulated cases, I compute the closet water pump. This partitions the simulated cases into clusters that reflect the selected pump neighborhoods. The figure below plots 1709 simulated cases for Broad Street pump neighborhood (pump \#7).

![](pump.neighborhoods.notes_files/figure-markdown_github/cloud-1.png)

The next step is to find the points along the periphery of the cluster.

![](pump.neighborhoods.notes_files/figure-markdown_github/perimeter-1.png)

Eventually, this will allow me to use graphics::polygon() to create plots like the Marshall Street example:

![](pump.neighborhoods.notes_files/figure-markdown_github/marshall-1.png)

However, doing this is easier said than done. One could compute the convex hull of a neighborhood's points. But because the convex hull picks a space based on the most outlying points in the cloud, when there are concavities, points outside the neighborhood will fall within the polygon.

![](pump.neighborhoods.notes_files/figure-markdown_github/hull-1.png)

Another possibility, which I used in an earlier version of 'cholera', is the 'alphahull' package. But that not only requires tweaking a parameter, it also has an ACM license that CRAN doesn't like.

String of pearls
----------------

My workable but mechanical solution is the following. First, I identify candidate points along the periphery by eliminating simulated cases that has immediate neighbors at each of the 4 cardinal directions (i.e., North, South, East and West). Second, I then connect the dots in the "right" order by using [epicycles](https://en.wikipedia.org/wiki/Deferent_and_epicycle). This works as double loop. The outer loop, which assembles the vertices of the polygon, moves in clockwise fashion. The inner loop, which finds the next vertex, moves counterclockwise in order to capture concavities. A tertiary concern is the density of simulated cases. The algorithm can fail by getting stuck in dead ends or by skipping over points. As is often the case, more data can help. As a tradeoff between computational speed and functional robustness, I use 20K simulated cases.

![](pump.neighborhoods.notes_files/figure-markdown_github/pearl_string-1.png)
