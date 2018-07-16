#' Plot Euclidean path pump neighborhoods.
#'
#' Plots star graph from pump to its cases.
#' @param pump.subset Numeric. Vector of numeric pump IDs to select (subset) from the neighborhoods defined by "pump.select". Negative selection possible. NULL selects all pumps in "pump.select".
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. NULL selects all pumps.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param case.set Character. "observed" or "expected".
#' @param multi.core Logical or Numeric. TRUE uses parallel::detectCores(). FALSE uses one, single core. You can also specify the number logical cores. On Window, only "multi.core = FALSE" is available.
#' @return A base R graph.
#' @export
#' @examples
#' \dontrun{
#' neighborhoodEuclidean()
#' neighborhoodEuclidean(-6)
#' neighborhoodEuclidean(pump.select = 6:7)
#' }

neighborhoodEuclidean <- function(pump.subset = NULL, pump.select = NULL,
  vestry = FALSE, case.set = "observed", multi.core = FALSE) {

  if (case.set %in% c("observed", "expected") == FALSE) {
    stop('"case.set" must be "observed" or "expected".')
  }

  cores <- multiCore(multi.core)

  if (vestry) {
    pump.data <- cholera::pumps.vestry
  } else {
    pump.data <- cholera::pumps
  }

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
    snow.colors <- cholera::snowColors(vestry = TRUE)
  } else {
    if (vestry) {
      if (any(abs(pump.select) %in% 1:14) == FALSE) {
        stop('With "vestry = TRUE", 1 >= |"pump.select"| <= 14')
      }
    } else {
      if (any(abs(pump.select) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", 1 >= |"pump.select"| <= 13')
      }
    }

    if (all(pump.select > 0)) {
      pump.id <- pump.data$id[pump.select]
      snow.colors <- cholera::snowColors(vestry = TRUE)[pump.select]
    } else if (all(pump.select < 0)) {
      sel <- pump.data$id %in% abs(pump.select) == FALSE
      pump.id <- pump.data$id[sel]
      snow.colors <- cholera::snowColors(vestry = TRUE)[sel]
    } else {
      stop('Use all positive or all negative "pump.select"')
    }
  }

  anchors <- cholera::fatalities.address$anchor.case

  if (vestry) {
    nearest.pump <- parallel::mclapply(anchors, function(x) {
      cholera::euclideanDistance(x, destination = pump.id, vestry = TRUE)$pump
    }, mc.cores = cores)
  } else {
    nearest.pump <- parallel::mclapply(anchors, function(x) {
      cholera::euclideanDistance(x, destination = pump.id)$pump
    }, mc.cores = cores)
  }

  if (is.null(pump.subset)) {
    out <- list(pump.data = pump.data,
                pump.select = pump.select,
                pump.id = pump.id,
                snow.colors = snow.colors,
                anchors = anchors,
                nearest.pump = unlist(nearest.pump))
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
      stop('Use all positive or all negative "pump.subset"!')
    }

    out <- list(pump.data = pump.data,
                pump.subset = pump.subset,
                pump.select = pump.select,
                pump.id = pump.id,
                snow.colors = snow.colors,
                anchors = anchors.subset,
                nearest.pump = unlist(nearest.pump.subset))
  }

  class(out) <- "euclidean"
  out
}

#' Plot method for neighborhoodWalking().
#'
#' @param x An object of class "euclidean" created by neighborhoodEuclidean().
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export

plot.euclidean <- function(x, ...) {
  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)
  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  pump.data <- x$pump.data
  pump.id <- x$pump.id
  snow.colors <- x$snow.colors
  anchors <- x$anchors
  pump.select <- x$pump.select
  pump.subset <- x$pump.subset
  nearest.pump <- x$nearest.pump

  plot(cholera::fatalities.address[, c("x", "y")], xlim = x.rng,
    ylim = y.rng, pch = NA, asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (is.null(x$pump.subset)) {
    invisible(lapply(seq_along(x$anchors), function(i) {
      p.data <- pump.data[pump.data$id == nearest.pump[[i]], ]
      sel <- cholera::fatalities.address$anchor.case %in% anchors[i]
      n.data <- cholera::fatalities.address[sel, ]
      n.color <- snow.colors[paste0("p", nearest.pump[[i]])]
      lapply(n.data$anchor.case, function(case) {
        c.data <- n.data[n.data$anchor.case == case, ]
        segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
          lwd = 0.5)
      })
    }))
  } else {
    if (all(pump.subset > 0)) {
      anchors.subset <- anchors[nearest.pump %in% pump.subset]
      nearest.pump.subset <- nearest.pump[nearest.pump %in% pump.subset]
    } else if (all(x$pump.subset < 0)) {
      anchors.subset <- anchors[nearest.pump %in%
        abs(pump.subset) == FALSE]
      nearest.pump.subset <- nearest.pump[nearest.pump %in%
        abs(pump.subset) ==- FALSE]
    } else {
      stop('Use all positive or all negative "pump.subset"!')
    }

    invisible(lapply(seq_along(anchors.subset), function(i) {
      p.data <- pump.data[pump.data$id == nearest.pump.subset[[i]], ]
      sel <- cholera::fatalities.address$anchor.case %in% anchors.subset[i]
      n.data <- cholera::fatalities.address[sel, ]
      n.color <- snow.colors[paste0("p", nearest.pump.subset[[i]])]
      lapply(n.data$anchor.case, function(case) {
        c.data <- n.data[n.data$anchor.case == case, ]
        segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
          lwd = 0.5)
      })
    }))
  }

  if (is.null(pump.select)) {
    points(pump.data[, c("x", "y")], pch = ". ")
    text(pump.data[, c("x", "y")], label = paste0("p", pump.id))
    title(main = "Pump Neighborhoods: Euclidean Paths (address)")
  } else {
    points(pump.data[pump.select, c("x", "y")], pch = ".")
    text(pump.data[pump.select, c("x", "y")], label = paste0("p", pump.id))
    title(main = paste0("Pump Neighborhoods: Euclidean Paths (address)", "\n",
      "Pumps ", paste(sort(pump.select), collapse = ", ")))
  }
}

#' Print method for neighborhoodWalking().
#'
#' @param x An object of class "euclidean" created by neighborhoodEuclidean().
#' @param ... Additional parameters.
#' @return An R class 'table' vector.
#' @export

print.euclidean <- function(x, ...) {
  if (class(x) != "euclidean") {
    stop('"x"\'s class needs to be "euclidean".')
  }
  print(table(x$nearest.pump))
}
