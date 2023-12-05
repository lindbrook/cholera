#' Create PDFs of landmarks not at road intersections
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param audit Logical. Plot visible graphic.
#' @param building.label Logical.
#' @param pch Integer. R pch. Default is "." or 46.
#' @noRd

landmarkPDF <- function(path = NULL, audit = FALSE, building.label = FALSE,
  pch = 46) {

  vars <- c("x", "y")
  proj <- paste0(vars, ".proj")
  lndmrk <- landmarkDataB()

  # select out landmarks at intersections
  inter.sel <- grepl("Square", lndmrk$name) | grepl("Pantheon", lndmrk$name)
  non.intersection <- lndmrk[!inter.sel, ]

  lab.sel <- non.intersection$x == non.intersection$x.proj
  labeled <- non.intersection[!lab.sel, ]

  if (building.label) {
    out <- labeled[, vars]
    pre <- "landmark.label"
  } else {
    out <- stats::setNames(non.intersection[, proj], vars)
    pre <- "landmark.proj"
  }

  if (audit) {
    snowMap(add.axes_box = TRUE, add.cases = FALSE, add.landmarks = FALSE,
      add.pumps = FALSE, add.roads = TRUE, add.frame = FALSE)
    points(out, col = "red", pch = pch)
  } else {
    grDevices::pdf(file = paste0(path, pre, ".pdf"))
    snowMap(add.axes_box = FALSE, add.cases = FALSE, add.landmarks = FALSE,
      add.pumps = FALSE, add.roads = FALSE, add.frame = FALSE)
    points(out, pch = pch)
    grDevices::dev.off()
  }
}

# cholera:::landmarkPDF(audit = TRUE, building.label = TRUE, pch = 1)
# cholera:::landmarkPDF(audit = TRUE, building.label = FALSE, pch = 1)
# cholera:::landmarkPDF(path = path, building.label = TRUE)
# cholera:::landmarkPDF(path = path, building.label = FALSE)
