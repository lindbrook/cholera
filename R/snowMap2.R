#' Plot John Snow's cholera map.
#'
#' latitude longitude prototype.
#' @return A base R graphics plot.
#' @export

snowMap2 <- function() {
  vars <- c("longitude", "latitude")
  plot(roads2[, vars], pch = NA)
  roads.list <- split(roads2[, vars], roads2$street)
  invisible(lapply(roads.list, lines, col = "gray"))
  points(fatalities2[, vars], pch = 15, cex = 0.5, col = "gray")
  points(pumps2[, vars], pch = 2, col = "blue")
  text(pumps2[, vars], pch = 2, col = "blue", labels = paste0("p", pumps2$id),
    pos = 1)
}
