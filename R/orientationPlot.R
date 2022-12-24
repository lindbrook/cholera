#' Orientation plot.
#'
#' @param pumpID Numeric. Selected pump ID.
#' @param metric Character. "eucldidean" or "walking".
#' @param pca.line Logical. Add PCA (TSS) line through data.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @export

orientationPlot <- function(pumpID = 6, metric = "euclidean",
  pca.line = FALSE, vestry = FALSE) {

  if (!vestry & pumpID %in% cholera::pumps$id == FALSE) {
    stop('For original pumps, pumpID must be a whole number between 1 and 13.',
      call. = FALSE)
  }

  if (vestry & pumpID %in% cholera::pumps.vestry$id == FALSE) {
    stop('For vestry pumps, pumpID must lie be a whole number 1 and 14.',
      call. = FALSE)
  }

  if (metric == "euclidean") fn <- neighborhoodVoronoi(vestry = vestry)
  else if (metric == "walking") fn <- neighborhoodWalking(vestry = vestry)

  vars <- c("x", "y")
  p7 <- cholera::pumps[cholera::pumps$id == 7, vars]
  pmp <- cholera::pumps[cholera::pumps$id == pumpID, vars]
  p.case <- pumpCase(fn)[[paste0("p", pumpID)]]

  if (vestry) {
    pmp <- cholera::pumps.vestry[cholera::pumps.vestry$id == pumpID, vars]
  } else {
    pmp <- cholera::pumps[cholera::pumps$id == pumpID, vars]    
  }

  snowMap(add.cases = FALSE, vestry = vestry)
  title(main = paste0("p", pumpID))
  segments(p7$x, p7$y, pmp$x, pmp$y, col = "blue", lwd = 1.25)

  if (metric == "euclidean") addVoronoi(vestry = vestry)
  else if (metric == "walking") addNeighborhoodWalking(vestry = vestry)

  no.obs.cases <- all(!cholera::fatalities$case %in% p.case)

  if (no.obs.cases) {
    title(sub = "No observed cases.")
  } else {
    p.data <- cholera::fatalities[cholera::fatalities$case %in% p.case, vars]
    points(p.data, pch = 16, col = "red", cex = 0.75)

    convex <- grDevices::chull(p.data)
    polygon(p.data[convex, ], border = "red")
    
    center <- data.frame(t(colMeans(p.data)))
    points(center, pch = "x", cex = 1.75, col = "forestgreen")

    if (pca.line) {
      pca <- stats::prcomp(p.data)
      b <- pca$rotation[2, 1] / pca$rotation[1, 1]
      a <- pca$center[2] - b * pca$center[1]
      abline(a, b, col = "red", lwd = 1.25, lty = "dashed")
    }
  }
}
