#' Create PDFs road segment endpoints stratified by number of intersections.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @export

subsetRoadsPDF <- function(path) {
  file.nm <- "road"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"

  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  dat <- framework
  dat$point.id <- paste0(dat$x, "-", dat$y)
  intersections <- table(dat$point.id)

  # > table(intersections)
  # intersections
  #   1   2   3   4
  # 276  10 221  44

  one <- cholera::rd.sample$one
  three <- cholera::rd.sample$three
  idx1 <- pointIndex(length(one), 25)
  idx3 <- pointIndex(length(three), 25)
  intersection.ct <- sort(unique(intersections))

  invisible(lapply(seq_along(intersection.ct), function(ct) {
    file.num <- paste0("0", ct)

    if (ct %in% c(1, 3)) {
      if (ct == 1) {
        idx <- idx1
        pts <- one
      } else if (ct == 3) {
        idx <- idx3
        pts <- three
      }

      sub.id <- letters[seq_along(idx$start)]

      lapply(seq_along(idx$start), function(i) {
        sel <- pts[idx[i, "start"]:idx[i, "stop"]]
        nm <- paste0(path, pre, paste0(file.num, sub.id[i]), post)
        grDevices::pdf(file = nm)
        plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
          xlab = NA, ylab = NA, bty = "n")
        points(dat[dat$point.id %in% sel, c("x", "y")], pch = 15, cex = 0.2)
        grDevices::dev.off()
      })
    } else {
      grDevices::pdf(file = paste0(path, pre, file.num, post))
      plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
        xlab = NA, ylab = NA, bty = "n")
      sel <- names(intersections[intersections == ct])
      points(dat[dat$point.id %in% sel, c("x", "y")], pch = 15, cex = 0.2)
      grDevices::dev.off()
    }
  }))
}
