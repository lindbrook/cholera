#' Create PDFs of pump orthogonal projections (prototype).
#'
#' For georeferencing in QGIS.
#' @param path Character. e.g., "~/Documents/Data/".
#' @param vestry Logical. Use vestry report pumps.
#' @param pch Numeric or Character.
#' @noRd

orthoProjPumpPDF <- function(path, vestry = FALSE, pch = 46) {
  if (vestry) {
    dat <- cholera::ortho.proj.pump.vestry
    pre <- "ortho.proj.pump.vestry"
  } else {
    dat <- cholera::ortho.proj.pump
    pre <- "ortho.proj.pump"
  }
  post <- ".pdf"
  file.nm <- paste0(path, pre, post)
  rng <- mapRange()
  grDevices::pdf(file = file.nm)
  plot(dat[, c("x.proj", "y.proj")], pch = pch, xaxt = "n", yaxt = "n",
    xlab = NA, ylab = NA, xlim = rng$x, ylim = rng$y, bty = "n", asp = 1)
  grDevices::dev.off()
}
