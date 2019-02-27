#' Locate water pump by numerical ID.
#'
#' Highlight selected water pump.
#' @param id Numeric or Integer. With \code{vestry = TRUE}, a whole number between 1 and 14. With \code{vestry = FALSE}, a whole number between 1 and 13. See \code{cholera::pumps.vestry} and \code{cholera::pumps} for IDs and details about specific pumps.
#' @param zoom Logical or Numeric. A numeric value >= 0 controls the degree of zoom. The default is 1.
#' @param vestry Logical. \code{TRUE} for the 14 pumps from Vestry Report. \code{FALSE} for the original 13 pumps.
#' @param add.title Logical. Include title.
#' @param highlight.segment Logical. Highlight case's segment.
#' @param data Logical. Output data.
#' @seealso\code{\link{pumpData}}
#' @return A base R graphics plot.
#' @export
#' @examples
#' pumpLocator()
#' pumpLocator(zoom = TRUE)
#' pumpLocator(14, vestry = TRUE, zoom = TRUE)

pumpLocator <- function(id = 7, zoom = 1,  vestry = FALSE, add.title = TRUE,
  highlight.segment = TRUE, data = FALSE) {

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
    p.data <- cholera::pumps.vestry
    ortho.data <- cholera::ortho.proj.pump.vestry
  } else {
    p.data <- cholera::pumps
    ortho.data <- cholera::ortho.proj.pump
  }

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)
  p.seg <- ortho.data[ortho.data$pump.id == id, "road.segment"]
  seg.data <- cholera::road.segments[cholera::road.segments$id == p.seg, ]

  if (data == FALSE) {
    if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
      if (is.logical(zoom)) {
        padding <- 0.1
        x.rng <- c(p.data[p.data$id == id, "x"] - padding,
                   p.data[p.data$id == id, "x"] + padding)
        y.rng <- c(p.data[p.data$id == id, "y"] - padding,
                   p.data[p.data$id == id, "y"] + padding)

      } else if (is.numeric(zoom)) {
        if (zoom >= 0) {
          x.rng <- c(p.data[p.data$id == id, "x"] - zoom,
                     p.data[p.data$id == id, "x"] + zoom)
          y.rng <- c(p.data[p.data$id == id, "y"] - zoom,
                     p.data[p.data$id == id, "y"] + zoom)
        } else stop("If numeric, zoom must be >= 0.")
      } else stop("zoom must either be logical or numeric.")
    } else {
      x.rng <- range(cholera::roads$x)
      y.rng <- range(cholera::roads$y)
    }

    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = 15, cex = 0.5, col = "lightgray", asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(p.data[p.data$id != id, c("x", "y")], pch = 2, cex = 1,
      col = "blue")
    points(p.data[p.data$id == id, c("x", "y")], pch = 17, cex = 1,
      col = "red")
    text(p.data[p.data$id == id, c("x", "y")],
      label = p.data$id[p.data$id == id], pos = 1, col = "red")

    if ((is.logical(zoom) & zoom == TRUE) | is.numeric(zoom)) {
      if (highlight.segment) {
        segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2,
          col = "red", lwd = 2)
      }

      if (add.title) {
        if (vestry) {
          title(main = paste0("Vestry Pump #", id, "; ", seg.data$name, " ",
            seg.data$id))
        } else {
          title(main = paste0("Pump #", id, "; ", seg.data$name, " ",
            seg.data$id))
        }
      }
    } else {
      if (add.title) {
        if (vestry) {
          title(main = paste0("Vestry Pump #", id, "; ", seg.data$name))
        } else {
          title(main = paste0("Pump #", id, "; ", seg.data$name))
        }
      }
    }
  } else list(pump.data = p.data[p.data$id == id, ], segment.data = seg.data)
}
