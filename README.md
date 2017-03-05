
<!-- README.md is generated from README.Rmd. Please edit that file -->
### cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 London cholera outbreak is one of the best known examples of data visualization:

![](vignettes/msu-snows-mapB.jpg)

However, as evidence of Snow's claims that cholera is a waterborne illness or that the Broad Street pump is the source of the outbreak, the map may not be as successful as is commonly thought.

To help assess such criticism and to allow people to analyze Snow's data for themselves, this package offers the following. First, it amends and augments [Dodson and Tobler](http://www.ncgia.ucsb.edu/pubs/snow/snow.html)'s 1992 digitization of Snow's map. Second, it allows users to compute and visualize *pump neighborhoods*, the set of residences closest to a given pump, based on Voronoi tessellation or walking distance. Third, it allows users to locate and visualize individual cases, pumps, roads and walking paths.

### example

The function below computes and plots the 10 walking distance *pump neighborhoods* for the 578 recorded fatalities in Snow's map.

``` r
library(cholera)
plot(neighborhoodWalking())
```

![](README-walk-1.png)

### installation

``` r
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```
