#' Orientation plot.
#'
#' @param pump.id Numeric. Select pump focus.
#' @param pump.select Numeric. Pump neighborhoods.
#' @param cases Character. "address" or "orthogonal".
#' @param convex.hull Logical. Add convex hull.
#' @param metric Character. "euclidean" or "walking".
#' @param pca.line Logical. Add PCA (TSS) line through data.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @noRd

orientationPlot <- function(pump.id = 6, pump.select = NULL,
    cases = "orthogonal", convex.hull = TRUE, metric = "euclidean",
    pca.line = FALSE, vestry = FALSE) {

  if (!cases %in% c("address", "orthogonal")) {
    stop('cases must be "address" or "orthogonal".', call. = FALSE)
  }

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (!is.null(pump.select)) {
    pump.select <- selectPump(pump.data, pump.select = pump.select,
      vestry = vestry)
  }

  if (metric == "euclidean") {
    fn <- neighborhoodVoronoi(pump.select = pump.select, vestry = vestry)
  } else if (metric == "walking") {
    fn <- neighborhoodWalking(pump.select = pump.select, vestry = vestry)
  }

  vars <- c("x", "y")
  p7 <- pump.data[pump.data$id == 7, vars]

  pmp <- pump.data[pump.data$id == pump.id, vars]
  p.cases <- pumpCase(fn)[[paste0("p", pump.id)]]

  snowMap(add.cases = FALSE, add.pumps = FALSE, vestry = vestry)
  addPump(pump.select = pump.select)

  if (!is.null(pump.select)) {
    addPump(setdiff(pump.data$id, pump.select), col = "gray")
  }

  title(main = paste0("p", pump.id))
  segments(p7$x, p7$y, pmp$x, pmp$y, col = "blue", lwd = 1.25)

  if (metric == "euclidean") {
    addVoronoi(pump.select = pump.select, vestry = vestry)
  } else if (metric == "walking") {
    addNeighborhoodWalking(pump.select = pump.select, vestry = vestry)
  }

  no.obs.cases <- all(!cholera::fatalities$case %in% p.cases)

  if (no.obs.cases) {
    title(sub = "No observed cases.")
  } else {
    if (cases == "orthogonal") {
      vars2 <- c("x.proj", "y.proj")
      p.data <- cholera::ortho.proj[cholera::ortho.proj$case %in% p.cases, vars2]
    } else if (cases == "address") {
      p.data <- cholera::fatalities[cholera::fatalities$case %in% p.cases, vars]
    }

    points(p.data, pch = 16, col = "red", cex = 0.75)

    if (convex.hull) {
      convex <- grDevices::chull(p.data)
      polygon(p.data[convex, ], border = "red")
    }

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
