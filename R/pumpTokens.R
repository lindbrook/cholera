#' Add pump tokens to plot.
#'
#' @noRd

pumpTokens <- function(pump.select, vestry, case.set, snow.colors, type) {
  if (vestry) {
    dat <- cholera::pumps.vestry
  } else {
    dat <- cholera::pumps
  }

  if (case.set == "observed") {
    if (is.null(pump.select)) {
      points(dat[, c("x", "y")], pch = 24, lwd = 1.25, col = snow.colors)
      text(dat[, c("x", "y")], pos = 1, cex = 0.9, labels = paste0("p", dat$id))
    } else {
      if (all(pump.select > 0)) {
        sel <- dat$id %in% pump.select
      } else if (all(pump.select < 0)) {
        sel <- dat$id %in% abs(pump.select) == FALSE
      }
      points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25,
        col = snow.colors[sel])
      text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
        labels = paste0("p", dat$id[sel]))
    }

  } else if (case.set == "expected") {
    if (type %in% c("road", "star")) {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 1.25, bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id))
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25,
          bg = snow.colors[sel])
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id[sel]))
      }

    } else if (type %in% c("area.points", "area.polygons")) {
      if (is.null(pump.select)) {
        points(dat[, c("x", "y")], pch = 24, lwd = 1.25,
          col = "white", bg = snow.colors)
        text(dat[, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id))
      } else {
        if (all(pump.select > 0)) {
          sel <- dat$id %in% pump.select
        } else if (all(pump.select < 0)) {
          sel <- dat$id %in% abs(pump.select) == FALSE
        }
        points(dat[sel, c("x", "y")], pch = 24, lwd = 1.25, col = "white",
          bg = snow.colors[sel])
        text(dat[sel, c("x", "y")], pos = 1, cex = 0.9,
          labels = paste0("p", dat$id[sel]))
      }
    }
  }
}
