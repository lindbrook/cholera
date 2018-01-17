
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/cholera)](https://cran.r-project.org/package=cholera)
[![GitHub\_Status\_Badge](https://img.shields.io/badge/GitHub-0.2.9.9013-red.svg)](https://github.com/lindbrook/cholera/blob/master/NEWS)

### cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 cholera outbreak in the Soho area of London is one of the best known examples of data visualization and information design.

![](vignettes/msu-snows-mapB.jpg)

By plotting the number and location of fatalities on a map, Snow was able to do something that we take for granted today: visualizing the spatial distribution of cholera cases. To our modern eye, the pattern on the map is unmistakable. Snow's claims, that cholera is a waterborne disease and that the pump on Broad Street was the source of the outbreak, seem almost self-evident. And yet, despite the map both the authorities and Snow's colleagues in the medical and scientific communities were not convinced that Snow was right.

Beyond considerations of time and place, I would suggest that there are "scientific" reasons why the map failed to impress and why it still fails to do so. While the map does shows a concentration of fatalities around the Broad Street pump, it actually doesn't exclude rival explanation. The pattern is not clearly different from what airborne transmission (i.e., miasma) might look like. To address this problem, I would argue that this is the reason why Snow added a graphical annotation in a second, lesser-known version of the map, which was published in the official report on the outbreak:

![](vignettes/fig12-6.png)

### pump neighborhoods

This annotation outlines the Broad Street *pump neighborhood*, the residences that are, according to Snow, within "close" walking distance to the pump. The notion of a pump neighborhood is key to Snow's claims because it provides a prediction about where we should and, equally important, where we shouldn't expect to find cases. If water is cholera's mode of transmission and if water pumps located on the street are the primary source of drinking water, then most, if not all, fatalities should be found *within* a neighborhood. The disease should stop at the neighborhood's borders.

However, to accurately compute the Broad Street pump neighborhood means being able to compute the neighborhoods of surrounding pumps. To that end, this release builds on Snow's efforts by computing two different types of neighborhoods. The first uses Voronoi tessellation. It works by computing the Euclidean distances between pumps. While popular and easy to compute, its only drawback is that roads and walking distance play no role in the choice of pump: the method assumes that people can walk through walls to get to their preferred pump.

``` r
plot(neighborhoodVoronoi())
addLandmarks()
```

![](man/figures/README-voronoi-1.png)

The second method, which follows Snow's lead, computes neighborhoods based on the "actual" walking distance. While more accurate, it is computationally more demanding to compute than Voronoi tessellation. To do so, I transform the roads on the map into a network graph and turn the computation of walking distance into a graph theory problem. For each case (observed or simulated), I compute the shortest path to the nearest pump, weighted by road distance. Then by applying the "rinse and repeat" principle, the different pump neighborhoods emerge:

``` r
plot(neighborhoodWalking())
addLandmarks()
```

![](man/figures/README-walk-1.png)

To explore the data, you can consider a variety of scenarios by computing neighborhoods using any subset of pumps. Here's the result excluding the Broad Street pump.

``` r
plot(neighborhoodWalking(-7))
```

![](man/figures/README-walk7-1.png)

You can also explore "expected" neighborhoods:

``` r
plot(neighborhoodWalking(case.set = "expected"))
```

![](man/figures/README-expected-1.png)

Or highlight the area of "expected" neighborhoods:

``` r
plot(neighborhoodWalking(case.set = "expected"), area = TRUE)
```

![](man/figures/README-expected_area-1.png)

### other package features

-   Fixes three apparent coding errors in Dodson and Tobler's 1992 digitization of Snow's map.
-   "Unstacks" the data in two ways to improve analysis and visualization.
-   Adds the ability to overlay graphical features like kernel density, Voronoi diagrams, and notable landmarks (John Snow's residence, the Lion Brewery, etc.).
-   Includes a variety of functions to find and locate cases, roads, pumps and walking paths.
-   Appends actual street names to the roads data.
-   Includes the revised pump data used in the second version of Snow's map from the Vestry report. This includes the corrected location of the Broad Street pump.
-   Adds two different aggregate time series fatalities data sets, taken from the Vestry report.

### getting started

To install "cholera"" (v. 0.2.1) from CRAN:

``` r
install.packages("cholera")
```

To install the current development version (v. 0.2.9.9013) from GitHub:

``` r
# Note that you may need to install the 'devtools' package:
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```

Read the package's vignettes. They include detailed discussions about the data, the functions and the methods used to "fix" the data and to compute walking distances and neighborhoods.

### note

neighborhoodWalking() is computationally intensive. Using the development version on a single core of a 2.3 GHz Intel i7, plotting observed paths takes about 8 seconds while expected paths takes about 35 seconds.

When using the parallel implementation of the function (currently only available on Linux and Mac and which the developer strongly discourages against using in a GUI or embedded environment), these times fall to 6 and 15 seconds (on 4 physical or 8 logical cores).
