## cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 London cholera outbreak is one of the best known examples of data visualization. To facilitate analysis, the `cholera` package does the following.

First, it amends and augments [Dodson and Tobler](http://www.ncgia.ucsb.edu/pubs/snow/snow.html)'s 1992 digitization of Snow's map by fixing three apparent coding errors, "unstacking" the data to improve analysis, appending road names to the street data, including data from the second version of the map that adds a fourteenth pump and correctly relocates the Broad Street pump, adding landmarks of interest (e.g., Lion Brewery, St. James Workhouse, St. Luke's Church) and incorporating two different sets of aggregate time series fatalities data.

Second, it offers functions that allow you to compute and visualize the neighborhoods of selected pumps (i.e., the set of addresses and roads that are closest to a given pump) based on Voronoi tessellation and actual walking distances.

Third, it offers functions that allow you to find and visualize individual cases, pumps, roads and walking paths.

![](vignettes/walking.paths.graph8.all.png)

## Installation

```R
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```
