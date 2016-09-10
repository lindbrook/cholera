#' Locate well by numerical ID.
#'
#' Plots John Snow's map of the 1854 London cholera outbreak and highlights the
#' selected water pump.
#' @param pump.id Numeric or Integer. Whole number between 1 and 13.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom. Default value is 2.
#' @param data.source Character. "pumps" for original 13. "vestry" for the 14 from Vestry Report: Hanover Square pump and a relocated Broad Street pump.
#' @return A base R graphics plot.
#' @export
#' @examples
#' pumpLocator(7) # Broad Street Pump
#' pumpLocator(7, zoom = TRUE)
#' pumpLocator(7, zoom = TRUE, radius = 1)
#' pumpLocator(14, data.source = "vestry", zoom = TRUE, radius = 1)

pumpLocator <- function(pump.id, zoom = FALSE, radius = 2, data.source = "pumps") {
  if (is.numeric(pump.id) == FALSE) {
    stop("pump.id must be numeric.")
  }

  if (data.source == "pumps" & pump.id %in% unique(cholera::pumps$id) == FALSE) {
    stop("pump.id must lie between 1 and 13.")
  }

  if (data.source == "vestry" &
    pump.id %in% unique(cholera::pumps.vestry$id) == FALSE) {
      stop("pump.id must lie between 1 and 14.")
  }

  if (data.source == "pumps") well <- cholera::pumps
  if (data.source == "vestry") well <- cholera::pumps.vestry
  road <- cholera::roads
  death <- cholera::fatalities

  roads.list <- split(road[, c("x", "y")], road$street)

  if (zoom == FALSE) {
    plot(death[, c("x", "y")], xlim = range(road$x), ylim = range(road$y),
      pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(well[well$id != pump.id, c("x", "y")], pch = 2, cex = 1,
      col = "blue")
    points(well[well$id == pump.id, c("x", "y")], pch = 17, cex = 1,
      col = "red")
    text(well[well$id == pump.id, c("x", "y")],
      label = well$id[well$id == pump.id], pos = 1, col = "red")
    title(main = "Water Pump")

  } else {
    x.rng <- c(well[well$id == pump.id, "x"] - radius,
               well[well$id == pump.id, "x"] + radius)
    y.rng <- c(well[well$id == pump.id, "y"] - radius,
               well[well$id == pump.id, "y"] + radius)

    plot(death[, c("x", "y")], xlim = x.rng, ylim = y.rng, pch = 15, cex = 0.5,
      col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(well[well$id != pump.id, c("x", "y")], pch = 2, cex = 1,
      col = "blue")
    points(well[well$id == pump.id, c("x", "y")], pch = 17, cex = 1,
      col = "red")
    text(well[well$id == pump.id, c("x", "y")],
      label = well$id[well$id == pump.id], pos = 1, col = "red")
    title(main = "Water Pump")
  }
}
