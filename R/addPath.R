#' Add the shortest walking path between a selected cases or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of nodes.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. "unit" is meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default walking speed is 5 km/hr.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts.
#' @param unit.interval Numeric. Sets interval between posts: for "distance", the default is 50 meters; for "time", the default is 60 seconds.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note The function uses a case's "address" (i.e., a stack's "anchor" case) to compute distance. Time is computed using cholera::distanceTime(). Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network; they form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf". Arrow points represent mileposts or timeposts to the destination.
#' @return An R list with two elements: a character vector of path nodes and a data frame summary.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export

addPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5, zoom = TRUE, radius = 0.5,
  unit.posts = "distance", unit.interval = NULL, alpha.level = 1) {

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    observed = observed,
                    weighted = weighted,
                    vestry = vestry,
                    unit = unit,
                    time.unit = time.unit,
                    walking.speed = walking.speed)

  x <- do.call("walkingPath", arguments)

  if (is.na(x$alter.node)) {
    txt1 <- paste("Case", x$origin, "is part of an isolated subgraph.")
    txt2 <- "It (technically) has no neareast pump."
    stop(paste(txt1, txt2))
  }

  if (isFALSE(alpha.level > 0 | alpha.level <= 1)) {
    stop('"alpha.level" must be > 0 and <= 1')
  }

  colors <- cholera::snowColors(x$vestry)

  nodes <- x$nodes
  edges <- x$edges
  g <- x$g
  ego.node <- x$ego.node
  alter.node <- x$alter.node

  dat <- numericNodeCoordinates(x$path)

  if (x$type == "case-pump") {
    alter <- nodes[nodes$node == alter.node, "pump"]
    case.color <- colors[alter]

    if (x$observed) {
      origin.obs <- cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")]
    } else {
      origin.obs <- cholera::regular.cases[x$origin, ]
    }

  } else if (x$type == "cases") {
    alter <- nodes[nodes$node == alter.node, "anchor"]
    case.color <- "blue"

    if (x$observed) {
      origin.obs <- cholera::fatalities[cholera::fatalities$case == x$origin,
        c("x", "y")]
    } else {
      origin.obs <- cholera::regular.cases[x$origin, ]
    }

    if (is.null(x$destination)) {
      if (x$observed) {
        destination.obs <- cholera::fatalities[cholera::fatalities$case ==
          alter, c("x", "y")]
      } else {
        destination.obs <-  cholera::regular.cases[alter, ]
      }
    } else {
      id <- x$destination[x$sel]

      if (x$observed) {
        destination.obs <- cholera::fatalities[cholera::fatalities$case == id,
          c("x", "y")]
      } else {
        destination.obs <- cholera::regular.cases[id, ]
      }
    }

  } else if (x$type == "pumps") {
    alter <- nodes[nodes$node == alter.node, "pump"]
    case.color <- "blue"
  }

  if (x$type == "case-pump" | x$type == "cases") {
    points(origin.obs, col = "red")
  }

  if (x$type == "cases") {
    points(destination.obs, col = "red")
  }

  points(dat[1, c("x", "y")], col = case.color, pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)

  if (x$type %in% c("case-pump", "cases")) {
    if (zoom) {
      if (x$observed) {
        text(cholera::fatalities[cholera::fatalities$case == x$origin,
          c("x", "y")], labels = x$origin, pos = 1, col = "red")
        if (x$type == "cases") {
          text(cholera::fatalities[cholera::fatalities$case == x$data$caseB,
            c("x", "y")], labels = x$data$caseB, pos = 1, col = "red")
        }
      } else {
        text(cholera::regular.cases[x$origin, ], labels = x$origin, pos = 1,
          col = "red")
        if (x$type == "cases") {
          text(cholera::regular.cases[x$data$caseB, ], labels = x$data$caseB,
            pos = 1, col = "red")
        }
      }
    }
  }

  if (alpha.level != 1) {
    case.color <- grDevices::adjustcolor(case.color, alpha.f = alpha.level)
  }

  drawPath(x$path, case.color)

  if (x$time.unit == "hour") {
    nominal.time <- paste(round(x$data$time, 1), "hr")
  } else if (x$time.unit == "minute") {
    nominal.time <- paste(round(x$data$time, 1), "min")
  } else if (x$time.unit == "second") {
    nominal.time <- paste(round(x$data$time), "sec")
  }

  if (x$unit == "native") {
    d.unit <- "units;"
  } else if (x$unit == "meter") {
    d.unit <- "m;"
  } else if (x$unit == "yard") {
    d.unit <- "yd;"
  }

  if (unit.posts %in% c("distance", "time") == FALSE) {
    stop('"unit.posts" must be "distance" or "time".')
  } else {
    if (is.null(unit.interval)) {
      if (unit.posts == "distance")  {
        unit.interval <- 50
      } else if (unit.posts == "time") {
        unit.interval <- 60
      }
    } else {
      if (!is.numeric(unit.interval)) {
        stop('"unit.interval" must be numeric.')
      }
    }

    path <- rev(x$path)
    path.edge <- data.frame(node1 = path[1:(length(path) - 1)],
                            node2 = path[2:length(path)],
                            stringsAsFactors = FALSE)

    edge.data <- identifyEdges(path.edge, edges)

    if (unit.posts == "distance") {
      cumulative <- cholera::unitMeter(cumsum(edge.data$d), "meter")
    } else if (unit.posts == "time") {
      cumulative <- cholera::distanceTime(cumsum(edge.data$d), speed = x$speed)
    }

    total <- cumulative[length(cumulative)]
    posts <- seq(0, total, unit.interval)

    if (max(posts) > max(cumulative)) {
      posts <- posts[-length(posts)]
    }

    bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                       hi = cumulative)

    edge.select <- vapply(posts[-1], function(x) {
      which(vapply(seq_len(nrow(bins)), function(i) {
        x >= bins[i, "lo"] & x < bins[i, "hi"]
      }, logical(1L)))
    }, integer(1L))

    start.node <- edgeOrder(edge.data, path.edge)

    post.coordinates <- lapply(seq_along(edge.select), function(i) {
      sel.data <- edge.data[edge.select[i], ]

      if (start.node[edge.select[i]] == 1) {
        edge.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                                y = c(sel.data$y1, sel.data$y2))
      } else if (start.node[edge.select[i]] == 2) {
        edge.data <- data.frame(x = c(sel.data$x2, sel.data$x1),
                                y = c(sel.data$y2, sel.data$y1))
      }

      ols <- stats::lm(y ~ x, data = edge.data)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- atan(edge.slope)
      h <- (posts[-1][i] - bins[edge.select[i], "lo"]) /
            cholera::unitMeter(1, "meter")

      delta <- edge.data[2, ] - edge.data[1, ]

      # Quadrant I
      if (all(delta > 0)) {
        post.x <- edge.data[1, "x"] + abs(h * cos(theta))
        post.y <- edge.data[1, "y"] + abs(h * sin(theta))

      # Quadrant II
      } else if (delta[1] < 0 & delta[2] > 0) {
        post.x <- edge.data[1, "x"] - abs(h * cos(theta))
        post.y <- edge.data[1, "y"] + abs(h * sin(theta))

      # Quadrant III
      } else if (all(delta < 0)) {
        post.x <- edge.data[1, "x"] - abs(h * cos(theta))
        post.y <- edge.data[1, "y"] - abs(h * sin(theta))

      # Quadrant IV
      } else if (delta[1] > 0 & delta[2] < 0) {
        post.x <- edge.data[1, "x"] + abs(h * cos(theta))
        post.y <- edge.data[1, "y"] - abs(h * sin(theta))

      # I:IV
      } else if (delta[1] > 0 & delta[2] == 0) {
        post.x <- edge.data[1, "x"] + abs(h * cos(theta))
        post.y <- edge.data[1, "y"]

      # I:II
      } else if (delta[1] == 0 & delta[2] > 0) {
        post.x <- edge.data[1, "x"]
        post.y <- edge.data[1, "y"] + abs(h * sin(theta))

      # II:III
      } else if (delta[1] < 0 & delta[2] == 0) {
        post.x <- edge.data[1, "x"] - abs(h * cos(theta))
        post.y <- edge.data[1, "y"]

      # III:IV
      } else if (delta[1] == 0 & delta[2] < 0) {
        post.x <- edge.data[1, "x"]
        post.y <- edge.data[1, "y"] - abs(h * sin(theta))
      }

      data.frame(post = posts[-1][i], x = post.x, y = post.y,
        angle = theta * 180L / pi, row.names = NULL)
    })

    coords <- do.call(rbind, post.coordinates)
    arrow.data <- edge.data[edge.select, ]
    start <- start.node[edge.select]

    invisible(lapply(seq_len(nrow(arrow.data)), function(i) {
      if (start[i] == 1) {
        arrows(arrow.data[i, "x2"],
               arrow.data[i, "y2"],
               coords[i, "x"],
               coords[i, "y"],
               lwd = 2, length = 0.075, col = case.color, code = 2)
      } else if (start[i] == 2) {
        arrows(arrow.data[i, "x1"],
               arrow.data[i, "y1"],
               coords[i, "x"],
               coords[i, "y"],
               lwd = 2, length = 0.075, col = case.color, code = 2)
      }
    }))
  }
}
