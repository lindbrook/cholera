#' Plot John Snow's cholera map.
#'
#' latitude longitude prototype.
#' @return A base R graphics plot.
#' @export

snowMap2 <- function() {
  vars <- c("longitude", "latitude")
  plot(cholera::roads2[, vars], pch = NA)
  roads.list <- split(cholera::roads2[, vars], cholera::roads2$street)
  invisible(lapply(roads.list, lines, col = "gray"))
  points(cholera::fatalities2[, vars], pch = 15, cex = 0.5, col = "gray")
  points(cholera::pumps2[, vars], pch = 2, col = "blue")
  text(cholera::pumps2[, vars], pch = 2, col = "blue",
    labels = paste0("p", cholera::pumps2$id), pos = 1)
}
