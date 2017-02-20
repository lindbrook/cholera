## cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 London cholera outbreak is one of the best known examples of data visualization. However, as evidence of his claim that cholera is a waterborne illness or that the Broad Street pump is the source of the outbreak, the map is actually not as successful as is commonly thought.

This package is designed to facilitate the analysis of Snow's data. It does the following. First, it amends and augments [Dodson and Tobler](http://www.ncgia.ucsb.edu/pubs/snow/snow.html)'s 1992 digitization of Snow's map. Second, it offers the ability to compute and visualize any or all pump neighborhoods (e.g., all but the Broad Street pump). Third, it offers the ability to find and visualize individual cases, pumps, roads and walking paths.

![](vignettes/walking.paths.graph8.all.png)

## Installation

```R
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```
