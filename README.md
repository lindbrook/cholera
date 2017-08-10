
<!-- README.md is generated from README.Rmd. Please edit that file -->
### cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 cholera outbreak in London's Soho is one of the best known examples of data visualization and information design.

![](vignettes/msu-snows-mapB.jpg)

The reasons are two-fold. First, as evidence of his claim that cholera is transmitted by water rather than air, Snow used a map to plot the spatial relationship between the location of water pumps, the primary source of drinking water, and that of cholera fatalities. Second, as a way to illustrate both the count and location of cases, Snow used "stacks" of horizontal bars (the orientation reflects the location's street).

However, while the map shows a concentration of fatalities around the Broad Street pump, it actually doesn't do the best job of excluding rival explanation. The pattern we see is not clearly different from what airborne transmission might look like. To address this problem, Snow added a graphical annotation to a second, lesser-known version of the map published in the official report on the outbreak:

![](vignettes/fig12-6.png)

### pump neighborhoods

This annotation outlines the Broad Street *pump neighborhood*, the residences Snow claims are within "close" walking distance to the pump. The notion of a pump neighborhood is important because it provides a specific (testable) prediction about where we should expect to find cases: if water is cholera's mode of transmission and and if the water pumps located on the street are the primary source of drinking water, then most, if not all, fatalities should be found *within* the neighborhood. To put it simply, the disease should stop at the neighborhood's borders. In this way, pump neighborhoods can help distinguish waterborne from airborne patterns of disease transmission.

To that end, this package builds on Snow's work by offering systematic ways to compute pump neighborhoods. Doing so not only provides a way to replicate and validate Snow's efforts, it also allows people to explore and investigate the data for themselves.

This release includes two methods of computing neighborhoods. The first uses Voronoi tessellation. It works by computing the Euclidean distances between pumps. While popular and easy to compute, its only drawback is that roads and walking distance play no role in the choice of pump: the method assumes that people can walk through walls to get to their preferred pump.

``` r
plot(neighborhoodVoronoi())
```

![](man/figures/README-voronoi-1.png)

The second method, which actually follows Snow's lead, computes neighborhoods based on the "actual" walking distance along the streets of Soho. While more accurate, it is computationally more demanding to compute than Voronoi tessellation. To do so, I transform the roads on the map into a "social" graph and turn the computation of walking distance into a graph theory problem. For each case (observed or simulated), I compute the shortest weighted path to the nearest pump. Then by applying the "rinse and repeat" principle, the different pump neighborhoods emerge:

``` r
plot(neighborhoodWalking())
```

![](man/figures/README-walk-1.png)

To explore the data, you can consider a variety of scenarios by computing neighborhoods using any subset of pumps. By doing so, you can explore hypotheses like the possibility that the choice of pump is affected by water quality.

### other package features

-   Fixes three apparent coding errors in Dodson and Tobler's 1992 digitization of Snow's map.
-   "Unstacks" the data in two ways to improve analysis and visualization.
-   Adds the ability to overlay graphical features like kernel density, Voronoi diagrams, and notable landmarks (John Snow's residence, the Lion Brewery, etc.).
-   Includes a variety of functions to find and locate cases, roads, pumps and walking paths.
-   Appends actual street names to the roads data.
-   Includes the revised pump data used in the second version of Snow's map from the Vestry report. This includes the corrected location of the Broad Street pump.
-   Adds two different aggregate time series fatalities data from the Vestry report.

### getting started

To install 'cholera', use the expression below (you may need to install the 'devtools' package).

``` r
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```

Besides the help pages, the vignettes include detailed discussion about the data and functions:

``` r
vignette("duplicate.missing.cases")
vignette("unstacking.fatalities")
vignette("pump.neighborhoods")
vignette("roads")
vignette("time.series")
```

### note

neighborhoodWalking() is computationally intensive (1-2 minutes on a single core). To improve performance, seven basic configurations have been pre-computed (for details, see neighborhoodWalking()'s Help Page) and a parallel, multi-core implementation is available on Linux and Mac.
