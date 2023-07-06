#' Add pump tokens to plot.
#'
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param latlong Logical.
#' @noRd

pumpTokens <- function(x, type, latlong = FALSE) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (x$vestry) dat <- cholera::pumps.vestry
  else dat <- cholera::pumps
  
  all.data <- dat[, vars]
  all.labels <- paste0("p", dat$id)

  if (!is.null(x$pump.select)) p.obs <- sort(x$pump.id)
  
  if (inherits(x, "voronoi") | inherits(x, "latlongVoronoi")) {
    x$case.set <-  "observed"
  } 

  if (x$case.set == "observed") {
    if (inherits(x, "voronoi") | inherits(x, "latlongVoronoi")) {
      if (is.null(x$pump.select)) {
        points(all.data, pch = 2, lwd = 2, col = x$snow.colors)
        text(all.data, pos = 1, cex = 0.9, labels = all.labels)
      } else {
        obs <- dat$id %in% p.obs
        pos.data <- dat[obs, vars]
        neg.data <- dat[!obs, vars]
        pos.labels <- paste0("p", dat$id[obs])
        neg.labels <- paste0("p", dat$id[!obs])

        points(pos.data, pch = 2, lwd = 2, col = x$snow.colors[obs])
        text(pos.data, pos = 1, cex = 0.9, labels = pos.labels)
        points(neg.data, pch = 2, lwd = 2, col = "gray")
        text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
      }
    } else {
      if (is.null(x$pump.select)) {
        points(all.data, pch = 24, lwd = 2, col = x$snow.colors)
        text(all.data, pos = 1, cex = 0.9, labels = all.labels)
      } else {
        obs <- dat$id %in% p.obs
        pos.data <- dat[obs, vars]
        neg.data <- dat[!obs, vars]
        pos.labels <- paste0("p", dat$id[obs])
        neg.labels <- paste0("p", dat$id[!obs])

        points(pos.data, pch = 24, lwd = 2, col = x$snow.colors[obs])
        text(pos.data, pos = 1, cex = 0.9, labels = pos.labels)
        points(neg.data, pch = 24, lwd = 1, col = "gray")
        text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
      }
    }
  } else if (x$case.set == "expected") {
    if (is.null(x$pump.select)) {
      if (type == "roads") {
        points(all.data, pch = 24, lwd = 1.5, col = "black")
        text(all.data, pos = 1, cex = 0.9, labels = all.labels, col = "black")
      } else {
        points(all.data, pch = 24, bg = x$snow.colors, col = "white")
        text(all.data, pos = 1, cex = 0.9, labels = all.labels, col = "white")
      }
    } else {
      obs <- dat$id %in% p.obs
      pos.data <- dat[obs, vars]
      neg.data <- dat[!obs, vars]
      pos.labels <- paste0("p", dat$id[obs])
      neg.labels <- paste0("p", dat$id[!obs])

      if (type == "roads") {
        if (is.null(x$pump.select)) {
          points(all.data, pch = 24, col = "black")
          text(all.data, pos = 1, cex = 0.9, col = "black", labels = all.labels)
        } else {
          points(pos.data, pch = 24, col = "black")
          text(pos.data, pos = 1, cex = 0.9, labels = pos.labels, col = "black")
          points(neg.data, pch = 24, col = "gray")
          text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
        }
      } else if (type %in% c("area.points", "area.polygons", "star")) {
        if (is.null(x$pump.select)) {
          points(all.data, pch = 24, lwd = 2, bg = x$snow.colors, col = "white")
          text(all.data, pos = 1, cex = 0.9, col = "white", labels = all.labels)
        } else {
          points(pos.data, pch = 24, lwd = 2, bg = x$snow.colors[obs],
            col = "white")
          text(pos.data, pos = 1, cex = 0.9, col = "white", labels = pos.labels)
          points(neg.data, pch = 24, lwd = 1, col = "black")
          text(neg.data, pos = 1, cex = 0.9, col = "black", labels = neg.labels)
        }
      }
    }
  }
}
