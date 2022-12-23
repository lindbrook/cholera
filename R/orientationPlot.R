#' Orientation plot.
#'
#' @param pumpID Numeric. Selected pump ID.
#' @param metric Character. "eucldidean" or "walking".
#' @export

orientationPlot <- function(pumpID = 6, metric = "euclidean") {
  if (metric == "euclidean") fn <- neighborhoodVoronoi()
  else if (metric == "walking") fn <- neighborhoodWalking()

  vars <- c("x", "y")
  p7 <- cholera::pumps[cholera::pumps$id == 7, vars]
  pmp <- cholera::pumps[cholera::pumps$id == pumpID, vars]
  p.case <- pumpCase(fn)[[paste0("p", pumpID)]]

  pmp <- cholera::pumps[cholera::pumps$id == pumpID, vars]
  snowMap(add.cases = FALSE)
  title(main = paste0("p", pumpID))
  segments( p7$x, p7$y, pmp$x, pmp$y, col = "blue", lwd = 1.25)

  p.data <- cholera::fatalities[cholera::fatalities$case %in% p.case, vars]
  points(p.data, pch = 16, col = "red", cex = 0.75)

  convex <- grDevices::chull(p.data)
  polygon(p.data[convex, ], border = "red")

  if (metric == "euclidean") addVoronoi()
  else if (metric == "walking") addNeighborhoodWalking()

  pca <- stats::prcomp(p.data)
  b <- pca$rotation[2, 1] / pca$rotation[1, 1]
  a <- pca$center[2] - b * pca$center[1]
  abline(a, b, col = "red", lwd = 1.25)
}
