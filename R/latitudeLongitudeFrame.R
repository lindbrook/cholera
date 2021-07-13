#' Create PDFs of map frame points.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @export

subsetFramePDF <- function(path) {
  file.nm <- "frame"
  pre <- paste0(file.nm, ".")
  post <- ".pdf"

  pts <- cholera::frame.sample
  idx <- pointIndex(length(pts), 25)
  num.id <- seq_len(nrow(idx))

  if (any(num.id >= 10)) {
    num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
  } else {
    num.id <- paste0("0", num.id)
  }

  dat <- cholera::roads[cholera::roads$name != "Map Frame", ]

  invisible(lapply(seq_along(num.id), function(i) {
    grDevices::pdf(file = paste0(path, pre, num.id[i], post))
    plot(dat$x, dat$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA, ylab = NA,
      bty = "n")
    sel <- idx[i, "start"]:idx[i, "stop"]
    points(dat[dat$id %in% pts[sel], c("x", "y")], pch = 15, cex = 0.2)
    grDevices::dev.off()
  }))
}

#' Sample for map frame segment endpoints.
#'
#' @export

mapFrameSamples <- function() {
  dat <- cholera::roads[cholera::roads$name == "Map Frame", ]
  dat$point.id <- paste0(dat$x, "-", dat$y)
  sample(dat$id)
}

# frame.sample <- mapFrameSamples()
# usethis::use_data(frame.sample, overwrite = TRUE)
