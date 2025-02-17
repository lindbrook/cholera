#' Compute Voronoi pump neighborhoods.
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param location Character. "nominal" or "orthogonal". "nominal" uses the x-y coordinates of \code{fatalities.address}. "orthogonal"uses the x-y coordinates of \code{ortho.proj}.
#' @param polygon.vertices Logical. \code{TRUE} returns a list of x-y coordinates of the vertices of Voronoi cells. Useful for \code{sp::point.in.polygon()} as used in \code{print.voronoi()} method.
#' @importFrom deldir deldir
#' @importFrom sp point.in.polygon
#' @return An R list with 12 objects.
#' \itemize{
#'   \item{\code{pump.id}: vector of selected pumps}
#'   \item{\code{voronoi}: output from deldir::deldir().}
#'   \item{\code{snow.colors}: neighborhood color based on snowColors().}
#'   \item{\code{x.rng}: range of x for plot.}
#'   \item{\code{y.rng}: range of y for plot.}
#'   \item{\code{select.string}: description of "pump.select" for plot title.}
#'   \item{\code{expected.data}: expected neighborhood fatality counts, based on Voronoi cell area.}
#'   \item{\code{coordinates}: polygon vertices of Voronoi cells.}
#'   \item{\code{statistic.data}: observed neighborhood fatality counts.}
#'   \item{\code{pump.select}: "pump.select" from voronoiNominal().}
#'   \item{\code{statistic}: "statistic" from voronoiNominal().}
#'   \item{\code{vestry}: "vestry" from voronoiNominal().}
#' }
#' @noRd

voronoiNominal <- function(pump.select = NULL, vestry = FALSE,
  location = "nominal", polygon.vertices = FALSE) {

  if (location %in% c("orthogonal", "nominal") == FALSE) {
    stop('location must be "nominal" or "orthogonal".', call. = FALSE)
  }

  if (location %in% c("nominal", "orthogonal") == FALSE) {
    stop('location must be "nominal" or "orthogonal".', call. = FALSE)
  } else if (location == "orthogonal") {
    if (vestry) {
      pump.data <- cholera::ortho.proj.pump.vestry
      pump.data$street <- cholera::pumps.vestry$street
    } else {
      pump.data <- cholera::ortho.proj.pump
      pump.data$street <- cholera::pumps$street
    }
    sel <- names(pump.data) %in% c("x.proj", "y.proj", "pump.id")
    names(pump.data)[sel] <- c("x", "y", "id")
  } else if (location == "nominal") {
    if (vestry) {
      pump.data <- cholera::pumps.vestry
    } else {
      pump.data <- cholera::pumps
    }
  }

  pump.id <- selectPump(pump.data, pump.select = pump.select, vestry = vestry)

  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  snow.colors <- snowColors(vestry = vestry)

  if (is.null(pump.select)) {
    voronoi <- deldir::deldir(pump.data[, c("x", "y")], rw = c(x.rng, y.rng),
      suppressMsge = TRUE)
    select.string <- NULL
  } else {
    voronoi <- deldir::deldir(pump.data[pump.id, c("x", "y")],
      rw = c(x.rng, y.rng), suppressMsge = TRUE)
    select.string <- paste(sort(pump.id), collapse = ", ")
  }

  voronoi.order <- as.numeric(rownames(voronoi$summary))
  pump.sel <- pump.data$id

  if (is.null(pump.select)) {
    pump.number <- pump.sel[voronoi.order]
  } else {
    pump.number <- pump.sel[pump.select][voronoi.order]
  }

  expected.data <- data.frame(pump = pump.number,
                              area = voronoi$summary$dir.area,
                              pct = voronoi$summary$dir.wts)

  if (is.null(pump.select)) {
    coordinates <- voronoiPolygons(pump.data, cholera::roads)
  } else {
    coordinates <- voronoiPolygons(pump.data[pump.select,], cholera::roads)
  }

  names(coordinates) <- paste0("p", pump.number)

  if (location == "orthogonal") {
    statistic.data <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::ortho.proj$x.proj,
        cholera::ortho.proj$y.proj, cell$x, cell$y)
    })
  } else if (location == "nominal") {
    statistic.data <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
        cholera::fatalities.address$y, cell$x, cell$y)
    })
  }

  names(statistic.data) <- paste0("p", pump.number)

  out <- list(pump.id = pump.id,
              voronoi = voronoi,
              location = location,
              snow.colors = snow.colors,
              x.rng = x.rng,
              y.rng = y.rng,
              select.string = select.string,
              expected.data = expected.data,
              coordinates = coordinates,
              pump.data = pump.data,
              statistic.data = statistic.data,
              pump.select = pump.select,
              vestry = vestry,
              statistic = location)

  class(out) <- c("voronoi", "voronoi_nominal")

  if (polygon.vertices) {
    out$coordinates
  } else {
    out
  }
}

#' Plot Voronoi neighborhoods.
#'
#' @param x An object of class "voronoi" created by \code{voronoiNominal()}.
#' @param delaunay.voronoi Character "delaunay", "voronoi", or "both".
#' @param euclidean.paths Logical. Plot all Euclidean paths (star graph).
#' @param ... Additional plotting parameters.
#' @return A base R graph.
#' @seealso
#'
#' \code{voronoiNominal()}
#'
#' \code{addVoronoi()}
#'
#' @export

plot.voronoi_nominal <- function(x, delaunay.voronoi = "voronoi",
  euclidean.paths = FALSE, ...) {

  snowMap(add.cases = FALSE, add.pumps = FALSE)

  if (is.null(x$pump.select)) {
    if (x$location == "orthogonal") {
      title(main = "Pump Neighborhoods: Voronoi (orthogonal)")
    } else if (x$location == "nominal") {
      title(main = "Pump Neighborhoods: Voronoi (nominal)")
    }
  } else {
    if (x$location == "orthogonal") {
      title(main = paste0("Pump Neighborhoods: Voronoi (orthogonal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$location == "nominal") {
      title(main = paste0("Pump Neighborhoods: Voronoi (nominal)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    }
  }

  pumpTokens(x, NULL)

  if (delaunay.voronoi == "voronoi") {
    plot(x$voronoi, add = TRUE, wline = "tess", showpoints = FALSE,
      cmpnt_lty = "solid")
  } else if (delaunay.voronoi == "delaunay") {
    plot(x$voronoi, add = TRUE, wline = "triang", showpoints = FALSE)
  } else if (delaunay.voronoi == "both") {
    plot(x$voronoi, add = TRUE, wline = "tess", showpoints = FALSE,
      cmpnt_lty = "solid")
    plot(x$voronoi, add = TRUE, wline = "triang", showpoints = FALSE)
  } else {
    stop('delaunay.voronoi must be "delaunay", "voronoi", or "both".',
      call. = FALSE)
  }

  voronoi.case.id <- pumpCase(x)
  voronoi.colors <- vector(length = length(unlist(voronoi.case.id)))

  if (x$location == "orthogonal") {
    names(voronoi.colors) <- cholera::ortho.proj$case
  } else if (x$location == "nominal") {
    names(voronoi.colors) <- cholera::fatalities.address$anchor
  }

  for (nm in names(voronoi.case.id)) {
    id <- voronoi.case.id[[nm]]
    voronoi.colors[names(voronoi.colors) %in% id] <- x$snow.colors[nm]
  }

  if (euclidean.paths) {
    if (x$vestry) pump.data <- cholera::pumps.vestry
    else pump.data <- cholera::pumps
    invisible(lapply(names(voronoi.case.id), function(nm) {
      p.data <- pump.data[paste0("p", pump.data$id) == nm, ]
      if (x$location == "orthogonal") {
        sel <- voronoi.case.id[[nm]] %in% cholera::fatalities.address$anchor
        n.anchor <- voronoi.case.id[[nm]][sel]
        n.data <- cholera::ortho.proj[cholera::ortho.proj$case %in% n.anchor, ]
      } else if (x$location == "nominal") {
        sel <- cholera::fatalities.address$anchor %in% voronoi.case.id[[nm]]
        n.data <- cholera::fatalities.address[sel, ]
        names(n.data)[names(n.data) == "nominal"] <- "case"
      }

      n.color <- x$snow.colors[nm]

      invisible(lapply(n.data$case, function(case) {
        c.data <- n.data[n.data$case == case, ]
        segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
          lwd = 0.5)
      }))
    }))
  } else {
    if (x$location == "orthogonal") {
      points(cholera::ortho.proj[, c("x.proj", "y.proj")], col = voronoi.colors,
        pch = 20, cex = 0.75)
    } else if (x$location == "nominal") {
      points(cholera::fatalities.address[, c("x", "y")], col = voronoi.colors,
        pch = 20, cex = 0.75)
    }
  }
}

#' Print method for voronoiNominal().
#'
#' Parameter values for voronoiNominal().
#' @param x An object of class "voronoi" created by \code{voronoiNominal()}.
#' @param ... Additional arguments.
#' @return A list of argument values.
#' @export

print.voronoi_nominal <- function(x, ...) {
  print(x[c("pump.id", "location", "vestry", "statistic")])
}

#' Summary method for voronoiNominal().
#'
#' Return computed counts for Voronoi neighborhoods.
#' @param object Object. An object of class "voronoi" created by \code{voronoiNominal()}.
#' @param ... Additional arguments.
#' @return A vector of counts by neighborhood.
#' @seealso \code{addVoronoi()}
#' \code{plot.voronoi()}
#' @export

summary.voronoi_nominal <- function(object, ...) {
  census <- vapply(object$statistic.data, sum, numeric(1L))
  stats::setNames(census, paste0("p", object$pump.id))
}
