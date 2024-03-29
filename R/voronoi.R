#' Compute Voronoi pump neighborhoods.
#'
#' Group cases into neighborhoods using Voronoi tessellation.
#' @param pump.select Numeric. Vector of numeric pump IDs to define pump neighborhoods (i.e., the "population"). Negative selection possible. \code{NULL} selects all pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry report. \code{FALSE} uses the 13 in the original map.
#' @param case.location Character. "address" or "anchor". "address" uses the x-y coordinates of \code{ortho.proj}. "anchor" uses the x-y coordinates of \code{fatalities.address}.
#' @param pump.location Character. "address" or "nominal". "address" uses the x-y coordinates of \code{ortho.proj.pump} or \code{ortho.proj.pump.vestry}. "nominal" uses the x-y coordinates of \code{pumps} or \code{pumps.vestry}.
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
#'   \item{\code{pump.select}: "pump.select" from neighborhoodVoronoi().}
#'   \item{\code{statistic}: "statistic" from neighborhoodVoronoi().}
#'   \item{\code{vestry}: "vestry" from neighborhoodVoronoi().}
#' }
#' @export
#' @examples
#' neighborhoodVoronoi()
#' neighborhoodVoronoi(vestry = TRUE)
#' neighborhoodVoronoi(pump.select = 6:7)
#' neighborhoodVoronoi(pump.select = -6)
#' neighborhoodVoronoi(pump.select = -6, polygon.vertices = TRUE)
#'
#' # coordinates for vertices also available in the returned object.
#' dat <- neighborhoodVoronoi(pump.select = -6)
#' dat$coordinates

neighborhoodVoronoi <- function(pump.select = NULL, vestry = FALSE,
  case.location = "anchor", pump.location = "nominal",
  polygon.vertices = FALSE) {

  if (case.location %in% c("address", "anchor") == FALSE) {
    stop('case.location must be "address" or "anchor".', call. = FALSE)
  } else {
    if (case.location == "address") statistic <- "address"
    else if (case.location == "anchor") statistic <- "anchor"
  }

  if (pump.location == "address") {
    if (vestry) {
      pump.data <- cholera::ortho.proj.pump.vestry
      pump.data$street <- cholera::pumps.vestry$street
    } else {
      pump.data <- cholera::ortho.proj.pump
      pump.data$street <- cholera::pumps$street
    }
    sel <- names(pump.data) %in% c("x.proj", "y.proj", "pump.id")
    names(pump.data)[sel] <- c("x", "y", "id")
  } else if (pump.location == "nominal") {
    if (vestry) {
      pump.data <- cholera::pumps.vestry
    } else {
      pump.data <- cholera::pumps
    }
  }

  if (is.null(pump.select) == FALSE) {
    if (is.numeric(pump.select) == FALSE) {
      stop("pump.select must be numeric.", call. = FALSE)
    }
    p.count <- nrow(pump.data)
    p.ID <- seq_len(p.count)
    if (any(abs(pump.select) %in% p.ID == FALSE)) {
      stop('With vestry = ', vestry, ', 1 >= |pump.select| <= ', p.count,
        call. = FALSE)
    }
    msg1 <- 'If specified,'
    msg2 <- "'pump.select' must include at least 2 different pumps."
    if (length(unique(p.ID[pump.select])) < 2) {
      stop(paste(msg1, msg2), call. = FALSE)
    }
  }

  if (is.null(statistic) == FALSE) {
    if (all(statistic %in% c("address", "anchor")) == FALSE) {
      stop('If specified, statistic must either be "address" or "anchor".',
        call. = FALSE)
    }
  }

  x.rng <- range(cholera::roads$x)
  y.rng <- range(cholera::roads$y)

  snow.colors <- snowColors(vestry = vestry)

  if (is.null(pump.select)) {
    pump.id <- pump.data$id
    voronoi <- deldir::deldir(pump.data[, c("x", "y")], rw = c(x.rng, y.rng),
      suppressMsge = TRUE)
    select.string <- NULL
  } else {
    pump.id <- pump.data$id[pump.select]
    voronoi <- deldir::deldir(pump.data[pump.select, c("x", "y")],
      rw = c(x.rng, y.rng), suppressMsge = TRUE)
    select.string <- paste(sort(pump.select), collapse = ", ")
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

  if (statistic == "address") {
    statistic.data <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::ortho.proj$x.proj,
        cholera::ortho.proj$y.proj, cell$x, cell$y)
    })
  } else if (statistic == "anchor") {
    statistic.data <- lapply(coordinates, function(cell) {
      sp::point.in.polygon(cholera::fatalities.address$x,
        cholera::fatalities.address$y, cell$x, cell$y)
    })
  }

  names(statistic.data) <- paste0("p", pump.number)

  output <- list(pump.id = pump.id,
                 voronoi = voronoi,
                 case.location = case.location,
                 snow.colors = snow.colors,
                 x.rng = x.rng,
                 y.rng = y.rng,
                 select.string = select.string,
                 expected.data = expected.data,
                 coordinates = coordinates,
                 statistic.data = statistic.data,
                 pump.select = pump.select,
                 vestry = vestry,
                 statistic = statistic)

  class(output) <- "voronoi"

  if (polygon.vertices) {
    output$coordinates
  } else {
    output
  }
}

#' Plot Voronoi neighborhoods.
#'
#' @param x An object of class "voronoi" created by \code{neighborhoodVoronoi()}.
#' @param delaunay.voronoi Character "delaunay", "voronoi", or "both".
#' @param euclidean.paths Logical. Plot all Euclidean paths (star graph).
#' @param ... Additional plotting parameters.
#' @return A base R graph.
#' @seealso
#'
#' \code{neighborhoodVoronoi()}
#'
#' \code{addVoronoi()}
#'
#' @export
#' @examples
#' plot(neighborhoodVoronoi())

plot.voronoi <- function(x, delaunay.voronoi = "voronoi", 
  euclidean.paths = FALSE, ...) {

  snowMap(add.cases = FALSE, add.pumps = FALSE)

  if (is.null(x$pump.select)) {
    if (x$case.location == "address") {
      title(main = "Pump Neighborhoods: Voronoi (address)")
    } else if (x$case.location == "anchor") {
      title(main = "Pump Neighborhoods: Voronoi (anchor)")
    }
  } else {
    if (x$case.location == "address") {
      title(main = paste0("Pump Neighborhoods: Voronoi (address)", "\n",
        "Pumps ", paste(sort(x$pump.select), collapse = ", ")))
    } else if (x$case.location == "anchor") {
      title(main = paste0("Pump Neighborhoods: Voronoi (anchor)", "\n",
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

  if (x$case.location == "address") {
    names(voronoi.colors) <- cholera::ortho.proj$case
  } else if (x$case.location == "anchor") {
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
      if (x$case.location == "address") {
        sel <- voronoi.case.id[[nm]] %in% cholera::fatalities.address$anchor
        n.anchor <- voronoi.case.id[[nm]][sel]
        n.data <- cholera::ortho.proj[cholera::ortho.proj$case %in% n.anchor, ]
      } else if (x$case.location == "anchor") {
        sel <- cholera::fatalities.address$anchor %in% voronoi.case.id[[nm]]
        n.data <- cholera::fatalities.address[sel, ]
        names(n.data)[names(n.data) == "anchor"] <- "case"
      }

      n.color <- x$snow.colors[nm]

      invisible(lapply(n.data$case, function(case) {
        c.data <- n.data[n.data$case == case, ]
        segments(c.data$x, c.data$y, p.data$x, p.data$y, col = n.color,
          lwd = 0.5)
      }))
    }))
  } else {
    if (x$case.location == "address") {
      points(cholera::ortho.proj[, c("x.proj", "y.proj")], col = voronoi.colors,
        pch = 20, cex = 0.75)
    } else if (x$case.location == "anchor") {
      points(cholera::fatalities.address[, c("x", "y")], col = voronoi.colors,
        pch = 20, cex = 0.75)
    }
  }
}

#' Print method for neighborhoodVoronoi().
#'
#' Parameter values for neighborhoodVoronoi().
#' @param x An object of class "voronoi" created by \code{neighborhoodVoronoi()}.
#' @param ... Additional arguments.
#' @return A list of argument values.
#' @export
#' @examples
#' neighborhoodVoronoi()
#' print(neighborhoodVoronoi())

print.voronoi <- function(x, ...) {
  print(x[c("pump.id", "case.location", "vestry", "statistic")])
}

#' Summary method for neighborhoodVoronoi().
#'
#' Return computed counts for Voronoi neighborhoods.
#' @param object Object. An object of class "voronoi" created by \code{neighborhoodVoronoi()}.
#' @param ... Additional arguments.
#' @return A vector of counts by neighborhood.
#' @seealso \code{addVoronoi()}
#' \code{plot.voronoi()}
#' @export
#' @examples
#' summary(neighborhoodVoronoi())

summary.voronoi <- function(object, ...) {
  census <- vapply(object$statistic.data, sum, numeric(1L))
  stats::setNames(census, paste0("p", object$pump.id))
}
