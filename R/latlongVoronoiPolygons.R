#' Create PDFs of Voronoi Polygon Vertices.
#'
#' For QGIS georeferencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param vestry Logical.
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

voronoiPDF <- function(path, vestry = FALSE, pch = 46, cex = 1) {
  file.nm <- "voronoi.polygon"
  post <- ".pdf"
  frame.corners <- data.frame(x = range(cholera::frame.data$x),
                              y = range(cholera::frame.data$y))

  if (vestry) {
    dat <- cholera::pumps.vestry
    pre <- paste0(file.nm, ".vestry")
  } else {
    dat <- cholera::pumps
    pre <- paste0(file.nm)
  }

  vertices <- cholera::voronoiPolygons(dat[, c("x", "y")],
    rw.data = frame.corners)

  rng <- mapRange()

  invisible(lapply(seq_along(vertices), function(i) {
    dat <- vertices[[i]]
    i <- ifelse(i < 10, paste0("0", i), paste(i))
    grDevices::pdf(file = paste0(path, pre, i, post))
    plot(dat[, c("x", "y")], pch = pch, cex = cex, xaxt = "n", yaxt = "n",
      xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y, asp = 1)
    grDevices::dev.off()
  }))
}
