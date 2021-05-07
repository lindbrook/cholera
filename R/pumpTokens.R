#' Add pump tokens to plot.
#'
#' @noRd

pumpTokens <- function(x, type) {
  if (x$vestry) dat <- cholera::pumps.vestry
  else dat <- cholera::pumps
  all.data <- dat[, c("x", "y")]
  all.labels <- paste0("p", dat$id)

  if (!is.null(x$pump.select)) {
    if (class(x) == "walking") {
      p.obs <- as.numeric(names(x$paths))
    } else if (class(x) == "euclidean") {
      p.obs <- sort(unique(x$nearest.pump))
    }
  }

  if (x$case.set == "observed") {
    if (is.null(x$pump.select)) {
      points(all.data, pch = 24, lwd = 2, col = x$snow.colors)
      text(all.data, pos = 1, cex = 0.9, labels = all.labels)
    } else {
      if (all(x$pump.select > 0)) {
        p.sel <- dat$id %in% x$pump.select
      } else if (all(x$pump.select < 0)) {
        p.sel <- dat$id %in% abs(x$pump.select) == FALSE
      } else stop('pump.select must be all positive or all negative.')

      obs <- dat$id %in% p.obs
      sel <- p.sel & obs
      pos.data <- dat[sel, c("x", "y")]
      neg.data <- dat[!sel, c("x", "y")]
      pos.labels <- paste0("p", dat$id[sel])
      neg.labels <- paste0("p", dat$id[!sel])

      points(pos.data, pch = 24, lwd = 2, col = x$snow.colors[sel])
      text(pos.data, pos = 1, cex = 0.9, labels = pos.labels)
      points(neg.data, pch = 24, lwd = 1, col = "gray")
      text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
    }
  } else if (x$case.set == "expected") {
    if (is.null(x$pump.select)) {
      points(all.data, pch = 24, lwd = 2, bg = x$snow.colors, col = "white")
      text(all.data, pos = 1, cex = 0.9, labels = all.labels, col = "white")
    } else {
      if (all(x$pump.select > 0)) {
        sel <- dat$id %in% x$pump.select
      } else if (all(x$pump.select < 0)) {
        sel <- dat$id %in% abs(x$pump.select) == FALSE
      } else stop('pump.select must be all positive or all negative.')

      pos.data <- dat[sel, c("x", "y")]
      pos.labels <- paste0("p", dat$id[sel])
      neg.data <- dat[!sel, c("x", "y")]
      neg.labels <- paste0("p", dat$id[!sel])

      if (type == "road") {
        if (is.null(x$pump.select)) {
          points(all.data, pch = 24, lwd = 2, bg = x$snow.colors)
          text(all.data, pos = 1, cex = 0.9, col = x$snow.colors,
            labels = all.labels)
        } else {
          points(pos.data, pch = 24, lwd = 2, bg = x$snow.colors[sel])
          text(pos.data, pos = 1, cex = 0.9, labels = pos.labels,
            col = x$snow.colors[sel])
          points(neg.data, pch = 24, lwd = 1, col = "gray")
          text(neg.data, pos = 1, cex = 0.9, col = "gray", labels = neg.labels)
        }
      } else if (type %in% c("area.points", "area.polygons", "star")) {
        if (is.null(x$pump.select)) {
          points(all.data, pch = 24, lwd = 2, bg = x$snow.colors, col = "white")
          text(all.data, pos = 1, cex = 0.9, col = "white", labels = all.labels)
        } else {
          points(pos.data, pch = 24, lwd = 2, bg = x$snow.colors[sel],
            col = "white")
          text(pos.data, pos = 1, cex = 0.9, col = "white", labels = pos.labels)
          points(neg.data, pch = 24, lwd = 1, col = "black")
          text(neg.data, pos = 1, cex = 0.9, col = "black", labels = neg.labels)
        }
      }
    }
  }
}
