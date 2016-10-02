![](walking.paths.graph8.all.png)

## cholera: amends and augments data from John Snow's 1854 cholera map

In his map of the 1854 London cholera outbreak, John Snow used stacked horizontal bars to represent the number of fatalities at individual addresses. In 1992, [Dodson and Tobler](http://www.ncgia.ucsb.edu/pubs/snow/snow.html) digitized Snow's map. These data, which are probably the most complete representation of the data in Snow's map, are preserved in the Michael Friendly's `HistData` package.

`cholera` amends and augments the Dodson and Tobler's data set in six ways: 1) it fixes three apparent coding errors; 2) it "unstacks" the data to facilitate numeric and statistical analysis; 3) it appends road names to the street data; 4) it adds two different sets of fatalities time series data; 5) it provides four versions of neighborhoods for the Broad Street pump, and two versions for all 13 or 14 recorded pumps; and 6) it adds landmarks of interest (e.g., Lion Brewery, St. James Workhouse, St. Luke's Church).

This is a development version.

## Installation

```R
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```
