#' Locate water pump by numerical ID.
#'
#' Highlight selected water pump.
#' @param id Numeric or Integer. With \code{vestry = TRUE}, a whole number between 1 and 14. With \code{vestry = FALSE}, a whole number between 1 and 13. See \code{cholera::pumps.vestry} and \code{cholera::pumps} for IDs and details about specific pumps.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @seealso\code{\link{pumpData}}
#' @return A base R graphics plot.
#' @export
#' @examples
#' pumpLocator(7) # Broad Street Pump
#' pumpLocator(7, zoom = TRUE)
#' pumpLocator(7, zoom = TRUE, radius = 1)
#' pumpLocator(14, vestry = TRUE, zoom = TRUE, radius = 1)

pumpLocator <- function(id, zoom = FALSE, radius = 2, vestry = FALSE) {
  if (is.numeric(id) == FALSE) {
    stop('id must be numeric.')
  }

  if (!vestry & id %in% cholera::pumps$id == FALSE) {
    stop('For original pumps, id must be a whole number between 1 and 13.')
  }

  if (vestry & id %in% cholera::pumps.vestry$id == FALSE) {
      stop('For vestry pumps, id must lie be a whole number 1 and 14.')
  }

  if (vestry) {
    well <- cholera::pumps.vestry
    title <- "Vestry Water Pumps"
  } else {
    well <- cholera::pumps
    title <- "Water Pumps"
  }

  road <- cholera::roads
  death <- cholera::fatalities
  roads.list <- split(road[, c("x", "y")], road$street)

  if (zoom) {
    x.rng <- c(well[well$id == id, "x"] - radius,
               well[well$id == id, "x"] + radius)
    y.rng <- c(well[well$id == id, "y"] - radius,
               well[well$id == id, "y"] + radius)

    plot(death[, c("x", "y")], xlim = x.rng, ylim = y.rng, pch = 15, cex = 0.5,
      col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(well[well$id != id, c("x", "y")], pch = 2, cex = 1,
      col = "blue")
    points(well[well$id == id, c("x", "y")], pch = 17, cex = 1,
      col = "red")
    text(well[well$id == id, c("x", "y")],
      label = well$id[well$id == id], pos = 1, col = "red")
    title(main = title)

  } else {
    plot(death[, c("x", "y")], xlim = range(road$x), ylim = range(road$y),
      pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(well[well$id != id, c("x", "y")], pch = 2, cex = 1,
      col = "blue")
    points(well[well$id == id, c("x", "y")], pch = 17, cex = 1,
      col = "red")
    text(well[well$id == id, c("x", "y")],
      label = well$id[well$id == id], pos = 1, col = "red")
    title(main = title)
  }
}
