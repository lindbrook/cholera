#' Plot Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.subset Numeric. Vector of numeric pump IDs to subset from the neighborhoods defined by \code{pump.select}. Negative selection possible. \code{NULL} selects all pumps in \code{pump.select}.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. On Windows, only \code{multi.core = FALSE} is available.
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#'
#' neighborhoodEuclidean()
#' neighborhoodEuclidean(-6)
#' neighborhoodEuclidean(pump.select = 6:7)
#' }

neighborhoodEuclidean <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, case.set = "observed", multi.core = FALSE) {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('case.set must be "observed" or "expected".')
  }

  cores <- multiCore(multi.core)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  p.count <- nrow(pump.data)
  p.ID <- seq_len(p.count)
  snow.colors <- cholera::snowColors(vestry = vestry)

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
  } else {
    if (is.numeric(pump.select) == FALSE) stop("pump.select must be numeric.")
    if (any(abs(pump.select) %in% p.ID) == FALSE) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count, ".")
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.data$id[pump.select]
      snow.colors <- snow.colors[pump.select]
    } else if (all(pump.select < 0)) {
      sel <- pump.data$id %in% abs(pump.select) == FALSE
      pump.id <- snow.colors[sel]
      snow.colors <- snow.colors[sel]
    } else {
      stop("Use all positive or all negative numbers for pump.select.")
    }
  }

  if (case.set == "observed") {
    anchors <- cholera::fatalities.address$anchor.case
    observed <- TRUE
  } else if (case.set == "expected") {
    anchors <- seq_len(nrow(cholera::regular.cases))
    observed <- FALSE
  }

  nearest.pump <- parallel::mclapply(anchors, function(x) {
    cholera::euclideanDistance(x, destination = pump.id,
      observed = observed, vestry = vestry)$pump
  }, mc.cores = cores)

  if (is.null(pump.subset)) {
    out <- list(pump.data = pump.data,
                pump.select = pump.select,
                case.set = case.set,
                pump.id = pump.id,
                snow.colors = snow.colors,
                anchors = anchors,
                observed = observed,
                nearest.pump = unlist(nearest.pump),
                cores = cores)
  } else {
    if (all(pump.subset > 0)) {
      anchors.subset <- anchors[unlist(nearest.pump) %in% pump.subset]
      nearest.pump.subset <- nearest.pump[unlist(nearest.pump) %in% pump.subset]
    } else if (all(pump.subset < 0)) {
      anchors.subset <- anchors[unlist(nearest.pump) %in%
        abs(pump.subset) == FALSE]
      nearest.pump.subset <- nearest.pump[unlist(nearest.pump) %in%
        abs(pump.subset) ==- FALSE]
    } else {
      stop('Use all positive or all negative numbers for "pump.subset".')
    }

    out <- list(pump.data = pump.data,
                pump.subset = pump.subset,
                pump.select = pump.select,
                case.set = case.set,
                pump.id = pump.id,
                snow.colors = snow.colors,
                anchors = anchors.subset,
                observed = observed,
                nearest.pump = unlist(nearest.pump.subset),
                cores = cores)
  }

  class(out) <- "euclidean"
  out
}

#' Plot method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param type Character. "star", "area.points" or "area.polygons". "area" flavors only valid when \code{case.set = "expected"}.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' \dontrun{
#'
#' plot(neighborhoodEuclidean())
#' plot(neighborhoodEuclidean(-6))
#' plot(neighborhoodEuclidean(pump.select = 6:7))
#' }

plot.euclidean <- function(x, type = "star", ...) {
  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }

  if (type %in% c("area.points", "area.polygons")) {
    if (x$case.set != "expected") {
      stop('area plots valid only when case.set = "expected".')
    }
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  pump.data <- x$pump.data
  pump.id <- x$pump.id
  anchors <- x$anchors
  pump.select <- x$pump.select
  pump.subset <- x$pump.subset
  nearest.pump <- x$nearest.pump

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng,
    ylim = y.rng, pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (type == "star") {
    invisible(lapply(seq_along(anchors), function(i) {
      p.data <- pump.data[pump.data$id == nearest.pump[[i]], ]
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      if (x$observed) {
        sel <- cholera::fatalities.address$anchor.case %in% anchors[i]
        n.data <- cholera::fatalities.address[sel, ]
        lapply(n.data$anchor.case, function(case) {
          c.data <- n.data[n.data$anchor.case == case, ]
          segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
            lwd = 0.5)
        })
      } else {
        n.data <- cholera::regular.cases[anchors[i], ]
        lapply(seq_len(nrow(n.data)), function(case) {
          c.data <- n.data[case, ]
          segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
            lwd = 0.5)
        })
      }
    }))

  } else if (type == "area.points") {
    invisible(lapply(seq_along(anchors), function(i) {
      # p.data <- pump.data[pump.data$id == nearest.pump[[i]], ]
      n.color <- x$snow.colors[paste0("p", nearest.pump[[i]])]
      if (x$observed) {
        sel <- cholera::fatalities.address$anchor.case %in% anchors[i]
        n.data <- cholera::fatalities.address[sel, ]
        lapply(n.data$anchor.case, function(case) {
          c.data <- n.data[n.data$anchor.case == case, ]
          points(c.data$x, c.data$y, col = n.color, pch = 15, cex = 1.25)
        })
      } else {
        n.data <- cholera::regular.cases[anchors[i], ]
        lapply(seq_len(nrow(n.data)), function(case) {
          c.data <- n.data[case, ]
          points(c.data$x, c.data$y, col = n.color, pch = 15, cex = 1.25)
        })
      }
    }))

  } else if (type == "area.polygons") {
    p.num <- sort(unique(nearest.pump))

    neighborhood.cases <- lapply(p.num, function(n) {
      which(nearest.pump == n)
    })

    periphery.cases <- parallel::mclapply(neighborhood.cases, peripheryCases,
      mc.cores = x$cores)
    pearl.string <- parallel::mclapply(periphery.cases, pearlString,
      mc.cores = x$cores)
    names(pearl.string) <- p.num

    invisible(lapply(names(pearl.string), function(nm) {
      sel <- paste0("p", nm)
      polygon(cholera::regular.cases[pearl.string[[nm]], ],
        col = grDevices::adjustcolor(x$snow.colors[sel], alpha.f = 2/3))
    }))
  }

  pumpTokens(x$pump.select, x$vestry, x$case.set, x$snow.colors, type)
  title(main = "Pump Neighborhoods: Euclidean")
}

#' Print method for neighborhoodEuclidean().
#'
#' @param x An object of class "euclidean" created by \code{neighborhoodEuclidean()}.
#' @param ... Additional parameters.
#' @return An R class 'table' vector.
#' @export

print.euclidean <- function(x, ...) {
  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }
  print(table(x$nearest.pump))
}
