## ---- echo = FALSE, message = FALSE--------------------------------------
library(cholera)

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot()

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot(output = "addresses")

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot(output = "fatalities")

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot(select = -6)

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot(select = -6, output = "fatalities")

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
voronoiPlot(select = -6, output = "addresses")

## ---- fig.width = 6, fig.height = 6, message = FALSE---------------------
walkingPathPlot(150)

