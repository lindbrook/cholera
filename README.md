
<!-- README.md is generated from README.Rmd. Please edit that file -->
### cholera: amend, augment and aid analysis of John Snow's 1854 cholera data

John Snow's map of the 1854 Soho, London cholera outbreak is one of the best known examples of data visualization and information design:

![](vignettes/msu-snows-mapB.jpg)

The "textbook" account is that the map helped Snow to identify the water pump that was the source of the disease, to stem the tide of the outbreak by convincing officials to remove that pump's handle, and, most importantly, to show that cholera is a waterborne not airborne disease. In fact, despite the quality of his evidence including the map, Snow actually failed to convince both local officials and his peers in the scientific community of the validity of his claims.

While their skepticism may seem wrong to our eyes (cholera is after all a waterborne disease), I would argue that a reassessment of the evidence would lead to a new, healthy skepticism: little of the "textbook" account stands up to scrutiny. That said, Snow's map is still relevant and important. It stands as a lesson about the challenges and difficulties of data visualization. To evaluate the evidence for yourself, this package provides tools to analyze, explore and visualize the map and its data.

### pump neighborhoods

The key to understanding what Snow hoped to achieve with his map lies with the second, lesser-known version that appeared in the official report on the outbreak:

![](vignettes/fig12-6.png)

What makes this version important is the addition of a graphical annotation that describes the Broad Street pump "neighborhood": the residences that were most likely to use the pump that Snow suspected as being the source of the outbreak. Because getting drinking water in 1854 London meant physically fetching it from a public pump, the notion of a "neighborhood" is pivotal to Snow's claim that cholera is a waterborne disease. If he was right, the outbreak should literally stop at the neighborhood's borders.

While the details of how Snow "computed" his annotation are lost to history, this package offers a variety of ways, including Snow's, to compute pump neighborhoods. The two more systematic methods compute the neighborhoods for all selected pumps. The first uses Voronoi tessellation, which is based on the Euclidean distance between pumps:

``` r
library(cholera)
plot(neighborhoodVoronoi())
```

![](README-voronoi-1.png)

While popular and easy to compute, the drawback is that with the Euclidean distance, or the distance "as-the-crow-flies", we assume that people can walk through walls.

The second method uses walking distances along the streets of Soho. While more accurate, this is harder to compute. To his credit, this appears to be Snow's method. He writes that the annotation includes "the various points which have been found by careful measurement to be at an equal distance by the nearest road from the pump in Broad Street and the surrounding pumps".

To replicate and extend his efforts, I wrote functions that compute walking distance pump neighborhoods. They work by transforming the roads on the map into a "social" graph and turning the computation of walking distances into a graph theory problem: the functions compute the shortest path between a case (observed or simulated) and its nearest pump:

``` r
walkingPath(150)
```

![](README-path-1.png)

"Rinse and repeat" for all observations and the different pump neighborhoods emerge:

``` r
plot(neighborhoodWalking())
```

![](README-walk-1.png)

One nice feature of these functions is that you can explore the data by including or excluding pumps. This can be important if factors other than distance play a role in the choice of pump. For example, Snow argued that water from the pump on Little Marlborough Street pump (\#6) was of low quality and that people in that neighborhood actually preferred the water from the Broad Street pump (\#7). To investigate this scenario, you simply exclude the pump on Little Marlborough Street (\#6):

``` r
plot(neighborhoodWalking(-6))
```

![](README-walk6-1.png)

### getting started

To install "cholera", use the expression below. Note that you need to have already installed the "devtools" R package.

``` r
# install.packages("devtools")
devtools::install_github("lindbrook/cholera", build_vignettes = TRUE)
```

Besides the help pages, the vignettes include detailed discussion about the data and functions included in this package:

``` r
vignette("duplicate.missing.cases")
vignette("unstacking.fatalities")
vignette("pump.neighborhoods")
vignette("roads")
vignette("time.series")
```
