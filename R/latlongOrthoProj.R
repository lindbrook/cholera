#' Create PDFs of orthogonal projection addresses.
#'
#' For QGIS geo-referencing.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param pch Integer. R pch.
#' @param cex Numeric.
#' @export

latlongOrthoAddressPDF <- function(path, pch = 46, cex = 1) {
  file.nm <- "ortho.address"
  post <- ".pdf"

  parts <- partitionOrthoAddresses()
  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rng <- mapRange()

  invisible(lapply(seq_along(parts), function(i) {
    dat <- cholera::ortho.proj[cholera::ortho.proj$case %in% parts[[i]], ]
    # all(parts[[i]] %in% cholera::ortho.proj$case)
    grDevices::pdf(file = paste0(path, file.nm, i, post))
    plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
      xlab = NA, ylab = NA, bty = "n", xlim = rng$x, ylim = rng$y)
    points(dat[, c("x.proj", "y.proj")], pch = pch, cex = cex)
    grDevices::dev.off()
  }))
}
