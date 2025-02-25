#' Add pump tokens to plot.
#'
#' @param x Object.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param alpha.level Numeric. Alpha level transparency for area plot: a value in [0, 1].
#' @param polygon.type Character. "border" or "solid".
#' @noRd

pumpTokens <- function(x, type, alpha.level, polygon.type) {
  if (x$latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  dat <- x$pump.data
  all.data <- dat[, vars]
  all.labels <- paste0("p", dat$id)

  if (inherits(x, "voronoi_nominal") | inherits(x, "voronoi_latlong")) {
    x$case.set <-  "observed"
  }

  if (x$case.set == "observed") {
    if (inherits(x, "voronoi_nominal") | inherits(x, "voronoi_latlong")) {
      if (is.null(x$pump.select)) {
        points(all.data, pch = 2, lwd = 2, col = x$snow.colors)
        text(all.data, pos = 1, cex = 0.9, labels = all.labels)
      } else {
        obs <- dat$id %in% x$pump.id
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
        obs <- dat$id %in% x$p.sel
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
        points(all.data, pch = 17, col = x$snow.colors)
        text(all.data, pos = 1, cex = 0.9, col = "black", labels = all.labels)

      } else if (type == "star") {
        points(all.data, pch = 24, bg = x$snow.colors, col = "white")
        text(all.data, pos = 1, cex = 0.9, col = "white", labels = all.labels)

      } else if (type == "area.points") {
        points(all.data, pch = 24, col = "white", bg = x$snow.colors)
        text(all.data, pos = 1, cex = 0.9, col = "white", labels = all.labels)

      } else if (type == "area.polygons") {
        bg.col <- grDevices::adjustcolor(x$snow.colors, alpha.f = alpha.level)

        if (polygon.type == "solid") {
          points(all.data, pch = 24, col = "white", bg = bg.col)
          text(all.data, pos = 1, cex = 0.9, col = "white", labels = all.labels)
        } else if (polygon.type == "border") {
          points(all.data, pch = 24, col = "black", bg = bg.col)
          text(all.data, pos = 1, cex = 0.9, col = "black", labels = all.labels)
        }
      }
    } else {
      obs <- dat$id %in% x$p.sel
      pos.data <- dat[obs, vars]
      neg.data <- dat[!obs, vars]
      pos.labels <- paste0("p", dat$id[obs])
      neg.labels <- paste0("p", dat$id[!obs])

      if (type == "roads") {
        points(pos.data, pch = 17, col = x$snow.colors[obs])
        text(pos.data, pos = 1, cex = 0.9, labels = pos.labels, col = "black")
        points(neg.data, pch = 24, col = "gray")
        text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)

      } else if (type == "star") {
        points(pos.data, pch = 24, bg = x$snow.colors[obs], col = "white")
        text(pos.data, pos = 1, cex = 0.9, labels = pos.labels, col = "white")
        points(neg.data, pch = 2, col = "black")
        text(neg.data, pos = 1, cex = 0.9, col = "black", labels = neg.labels)

      } else if (type == c("area.points")) {
        points(pos.data, pch = 24, col = "white", bg = x$snow.colors[obs])
        text(pos.data, pos = 1, cex = 0.9, col = "white", labels = pos.labels)
        points(neg.data, pch = 2)
        text(neg.data, pos = 1, cex = 0.9, col = "black", labels = neg.labels)

      } else if (type == "area.polygons") {
        bg.col <- grDevices::adjustcolor(x$snow.colors[obs],
          alpha.f = alpha.level)

        if (polygon.type == "solid") {
          points(pos.data, pch = 24, col = "white", bg = bg.col)
          text(pos.data, pos = 1, cex = 0.9, col = "white", labels = pos.labels)
          points(neg.data, pch = 24, col = "black")
          text(neg.data, pos = 1, cex = 0.9, col = "black", labels = neg.labels)
        } else if (polygon.type == "border") {
          points(pos.data, pch = 24, col = "black", bg = bg.col)
          text(pos.data, pos = 1, cex = 0.9, col = "black", labels = pos.labels)
          points(neg.data, pch = 24, col = "gray")
          text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
        }
      }
    }
  }
}
