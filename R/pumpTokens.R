#' Add pump tokens to plot.
#'
#' @noRd

pumpTokens <- function(pump.select, vestry, case.set, snow.colors, type, pths) {
  if (vestry) {
    dat <- cholera::pumps.vestry
  } else {
    dat <- cholera::pumps
  }

  if (case.set == "observed") {
    if (is.null(pump.select)) {
      points(dat[, c("x", "y")], pch = 24, lwd = 2, col = snow.colors)
      text(dat[, c("x", "y")], pos = 1, cex = 0.9, labels = paste0("p", dat$id))
    } else {
      if (all(pump.select > 0)) {
        sel <- dat$id %in% pump.select
      } else if (all(pump.select < 0)) {
        p.sel <- !dat$id %in% abs(pump.select)
        unobs <- dat$id %in% as.numeric(names(pths))
        sel <- p.sel & unobs
      }

      points(dat[sel, c("x", "y")], pch = 24, lwd = 2, col = snow.colors[sel])
      text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
        labels = paste0("p", dat$id[sel]))
      points(dat[!sel, c("x", "y")], pch = 24, lwd = 2, col = "gray")
      text(dat[!sel, c("x", "y")], pos = 1, cex = 0.9, col = "gray",
        labels = paste0("p", dat$id[!sel]))
    }

  } else if (case.set == "expected") {
    if (type == "road") {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 2, bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id), col = snow.colors)
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 2,
          bg = snow.colors[sel])
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id[sel]), col = snow.colors[sel])
        points(dat[!sel, c("x", "y")], pch = 24, lwd = 1, col = "gray")
        text(dat[!sel, c("x", "y")], pos = 1, cex = 0.9, col = "gray",
          labels = paste0("p", dat$id[!sel]))
      }

    } else if (type %in% c("area.points", "area.polygons")) {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 2,
          col = "white", bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id), col = "white")
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 2, col = "white",
          bg = "white")
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9, col = "white",
          labels = paste0("p", dat$id[sel]))
        points(dat[!sel, c("x", "y")], pch = 24, lwd = 1, col = "white")
        text(dat[!sel, c("x", "y")], pos = 1, cex = 0.9, col = "white",
          labels = paste0("p", dat$id[!sel]))
      }
    }
  }
}
