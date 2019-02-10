Pump Neighborhoods
================
lindbrook
2019-02-10

## Overview

John Snow actually published two versions of the cholera map. The first,
which appeared in *On The Mode Of Communication Of Cholera* (Snow
1855a), is the more famous. The second, which appeared in the *Report On
The Cholera Outbreak In The Parish Of St. James, Westminster, During The
Autumn Of 1854* (Snow 1855b), is the more important. What makes it
important is that Snow adds a graphical annotation that outlines the
neighborhood around the Broad Street pump, the set of addresses that he
contends is most likely to use the pump:

![](fig12-6.png)

By identifying the pump’s neighborhood, Snow sets limits on where we
should and where we should *not* find fatalities. Ideally, this would
help support his claims that cholera is a waterborne disease and that
the Broad Street pump is the source of the outbreak. Looking at the
second map Snow writes: “it will be observed that the deaths either very
much diminish, or cease altogether, at every point where it becomes
decidedly nearer to send to another pump than to the one in Broad
street” (Snow 1855b, 109).

To help assess whether the map supports Snow’s arguments, I provide
functions that allow you to analyze and visualize two flavors of pump
neighborhoods: Voronoi tessellation, which is based on the Euclidean
distance between pumps, and walking distance, which is based on the
paths traveled along the roads of Soho. In either case, the guiding
principle is the same: all else being equal, people will choose the
closest pump.

### Voronoi tessellation

Cliff and Haggett (1988) appear to be the first to use Voronoi
tessellation\[1\] to compute pump neighborhoods. In their digitization
of Snow’s map, they include coordinates for 13 Voronoi cells. These are
available in `HistData::Snow.polygons`. To replicate their effort, I use
`deldir::deldir()`. With the exception of the border between the
neighborhoods of the Market Place and the Adam and Eve Court pumps
(pumps \#1 and \#2), I find that Dodson and Tobler’s computation are
otherwise identical to those using ‘deldir’.

To explore the data using this approach, you can use
`neighborhoodVoronoi()` to create scenarios of different sets of
neighborhoods based on the pumps you select. The figure below plots the
321 fatality “addresses” and the Voronoi cells for the 13 pumps in the
original
map.

``` r
plot(neighborhoodVoronoi())
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

The next figure plots the same data but excludes the Broad Street pump
from
consideration.

``` r
plot(neighborhoodVoronoi(-7))
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

In either case, the numerical results can be summarized using the
`print()` method.

``` r
# print(neighborhoodVoronoi()) or
neighborhoodVoronoi()
>  [1]   0   1  10  13   3  39 182  12  17  38   2   2   2
```

To get an estimate of the difference between observed and expected
fatalities, you can use `pearsonResiduals()`. Note that “Pearson” is
“Count” minus “Expected” divided by the square root of “Expected”:

``` r
pearsonResiduals(neighborhoodVoronoi())
>    pump.id Count Percent  Expected    Pearson
> 1        1     0    0.00 19.491762 -4.4149476
> 2        2     1    0.31  6.234783 -2.0964669
> 3        3    10    3.12 13.983723 -1.0653142
> 4        4    13    4.05 30.413466 -3.1575647
> 5        5     3    0.93 26.463561 -4.5611020
> 6        6    39   12.15 39.860817 -0.1363445
> 7        7   182   56.70 27.189021 29.6896428
> 8        8    12    3.74 22.112085 -2.1504327
> 9        9    17    5.30 15.531264  0.3726836
> 10      10    38   11.84 18.976878  4.3668614
> 11      11     2    0.62 24.627441 -4.5595901
> 12      12     2    0.62 29.670993 -5.0799414
> 13      13     2    0.62 46.444206 -6.5215283
```

### Walking distance

The obvious criticism against using Voronoi tessellation to analyze
Snow’s map is that the neighborhoods it describes are based solely on
the Euclidean distance between water pumps. Roads and buildings don’t
matter. In this view of the world, people walk to water pumps in perfect
straight line fashion rather than along the twists and turns of paths
created by having to follow roads and streets.

Not only is this unrealistic, it’s also contrary to how Snow thought
about the problem. Snow’s graphical annotation appears to be based on a
computation of walking distance. He writes: “The inner dotted line on
the map shews \[sic\] the various points which have been found by
careful measurement to be at an equal distance by the nearest road from
the pump in Broad Street and the surrounding pumps …” (*Report On The
Cholera Outbreak In The Parish Of St. James, Westminster, During The
Autumn Of 1854*, p. 109.).

While the details of his computations seem to be lost to history, I
replicate and extend his efforts by writing functions that allow you to
compute and visualize pump neighborhoods based on walking distance.\[2\]
My implementation works by transforming the roads on the map into a
network graph and turning the computation of walking distance into a
graph theory problem. For each case (observed or simulated), I compute
the shortest path, weighted by the length of roads, to the nearest pump.
Then, by drawing the unique paths for all cases, a pump’s neighborhood
emerges:

``` r
plot(neighborhoodWalking())
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

The summary results are:

``` r
# print(neighborhoodWalking()) or
neighborhoodWalking()
>   3   4   5   6   7   8   9  10  11  12 
>  12   6   1  44 189  14  32  20   2   1
```

``` r
pearsonResiduals(neighborhoodWalking())
>    pump.id       Count Percent  Expected    Pearson
> 1        1    2.734490    7.76 463.67847 -21.406205
> 2        2    0.000000    0.45  26.88857  -5.185419
> 3        3  745.625767    5.61 335.21086  22.416308
> 4        4  352.936248    8.18 488.77447  -6.144234
> 5        5   85.019142    3.05 182.24476  -7.202001
> 6        6  867.605713   15.87 948.27028  -2.619490
> 7        7 1609.521386    9.64 576.01295  43.062367
> 8        8  529.692325    6.71 400.93847   6.430154
> 9        9  681.434119    7.04 420.65676  12.714695
> 10      10  721.772425    5.59 334.01581  21.216596
> 11      11  248.094923    7.57 452.32552  -9.602742
> 12      12  128.326946   10.25 612.46190 -19.562615
> 13      13    2.474584   12.26 732.56419 -26.974495
```

### “Expected” walking neighborhoods

To get a sense of the full range of a walking neighborhood or of the
“expected” neighborhood, I apply the approach above to simulated data.
Using `sp::spsample()` and `sp::Polygon()`, I place 20,000 regularly
spaced points, which lie approximately 6 meters apart,
`unitMeter(dist(regular.cases[1:2, ]))`, across the map and then compute
the shortest path to the nearest pump.\[3\]

I visualize the results in two ways. In the first, I identify
neighborhoods by coloring
roads.\[4\]

``` r
plot(neighborhoodWalking(case.set = "expected"))
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

In the second, I identify neighborhoods by coloring regions using
points() or polygon().\[5\] The points() approach, shown below, is
faster and more
robust.

``` r
plot(neighborhoodWalking(case.set = "expected"), type = "area.points")
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

### Exploring scenarios

Beyond comparing methods (e.g., walking v. Euclidean distance), you can
explore different scenarios. For example, Snow argues that residents
found the water from the Carnaby and Little Marlborough Street pump
(\#6) to be of low quality and actually preferred to go to the Broad
Street pump (\#7).\[6\] Using this package, you can explore this and
other possibilities by selecting or excluding
pumps:

``` r
plot(neighborhoodWalking(-6))
```

<img src="pump.neighborhoods_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

### Note on `neighborhoodWalking()`

`neighborhoodWalking()` is computationally intensive. Using R version
3.5.2 on a single core of a 2.3 GHz Intel i7, plotting observed paths to
PDF takes about 5 seconds; doing the same for expected paths takes about
30 seconds. Using the functions’ parallel implementation on 4 physical
(8 logical) cores, the times fall to about 4 and 13 seconds.

Note that parallelization is currently only available on Linux and Mac.

Also, note that although some precautions are taken in R.app on macOS,
the developers of the ‘parallel’ package, which `neighborhoodWalking()`
uses, strongly discourage against using parallelization within a GUI or
embedded environment. See `vignette("parallel")` for details.

## Notes

1.  <http://www.ams.org/samplings/feature-column/fcarc-voronoi>

2.  The computation of walking distance is by no means new (see Shiode,
    2012). Another approach is to use GIS. For applications that don’t
    need to consider the actual historic walking distances, this
    layers-based approach, which typically relies on current maps, may
    be sufficient: e.g.,
    <https://www.theguardian.com/news/datablog/2013/mar/15/john-snow-cholera-map>.
    To reconstruct the roads represented on Snow’s map, one might also
    consider John Mackenzie’ approach at
    <https://www1.udel.edu/johnmack/frec682/cholera/cholera2.html>.

3.  These data are found in `regular.cases`.

4.  Shiode (2012) uses this approach.

5.  Mackenzie (N.D) uses this area plot approach. Cliff and Haggett
    produce an adjusted Voronoi cells that reflect walking distances:
    “So far we have assumed that access to each pump was determined by
    ‘as-the-crow-flies’ distances. While the physical effort of carrying
    water mean that most people visited their nearest pump, recognition
    must be made of the complex street pattern in this area of London.
    Therefore in (D), the Thiessen polygons have been adjusted to take
    into account the patterns of access made possible by the street
    system shown in diagram (A)” (Cliff and Haggett 1988, 53). However,
    details about how this was done does not appear to be available.
    Moreover, because the graphic only shows the outline of the polygon
    and not the streets, comparisons with other approaches is difficult.

6.  Snow writes: “It requires to be stated that the water of the pump in
    Marlborough Street, at the end of Carnaby Street, was so impure that
    many persons avoided using it; and I found that the persons who died
    near this pump, in the beginning of September, had water from the
    Broad Street pump.”
