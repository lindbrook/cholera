#' Add the shortest walking path between a selected cases or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of nodes.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. \code{unit} is meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts.
#' @param unit.interval Numeric. Sets interval between posts: for "distance", the default is 50 meters; for "time", the default is 60 seconds.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note The function uses a case's "address" (i.e., a stack's "anchor" case) to compute distance. Time is computed using cholera::distanceTime(). Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network; they form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of \code{Inf}. Arrow points represent mileposts or timeposts to the destination.
#' @return An R list with two elements: a character vector of path nodes and a data frame summary.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export

addWalkingPath <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5, zoom = TRUE, radius = 0.5,
  unit.posts = "distance", unit.interval = NULL, alpha.level = 1) {

  if (is.numeric(origin) == FALSE) {
    stop('origin must be numeric.')
  }

  if (is.null(destination) == FALSE) {
    if (is.numeric(destination) == FALSE) {
      stop('destination must be numeric.')
    }
  }

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('unit must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('time.unit must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('type must be "case-pump", "cases" or "pumps".')
  }

  obs.ct <- nrow(cholera::fatalities)
  exp.ct <- nrow(cholera::regular.cases)

  if (observed) ct <- obs.ct else ct <- exp.ct

  if (vestry) {
    p.data <- cholera::pumps.vestry
  } else {
    p.data <- cholera::pumps
  }

  p.count <- nrow(p.data)
  p.ID <- seq_len(p.count)

  if (type == "case-pump") {
    if (origin %in% seq_len(ct) == FALSE) {
      txt1 <- 'With type = "'
      txt2 <- '" and observed = '
      txt3 <- ", 'origin' must be between 1 and "
      stop(txt1, type, txt2, observed, txt3, ct, ".")
    }

    if (is.null(destination) == FALSE) {
      if (any(abs(destination) %in% p.ID == FALSE)) {
        stop('With vestry = ', vestry, ", 1 >= |destination| <= ", p.count, ".")
      }
    }
  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% seq_len(ct) == FALSE)) {
      txt1 <- 'With type = '
      txt2 <- ' and observed = '
      txt3 <- ", the absolute value of 'origin' and 'destination' must fall "
      txt4 <- 'between 1 and '
      stop(txt1, type, txt2, observed, txt3, txt4, ct, ".")
    }
  } else if (type == "pumps") {
    if (any(abs(c(origin, destination)) %in% p.ID == FALSE)) {
      txt1 <- 'With type = "'
      txt2 <- '" and vestry = '
      txt3 <- ", 'origin' and 'destination' must whole numbers 1 >= |x| <= "
      stop(txt1, type, txt2, vestry, txt3, p.count, ".")
    }
  }

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
    txt2 <- "It (technically) has no nearest pump."
    stop(txt1, txt2)
  }

  if ((alpha.level > 0 & alpha.level <= 1) == FALSE) {
    stop('alpha.level must be > 0 and <= 1')
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
        destination.obs <- cholera::regular.cases[alter, ]
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

  # mileposts #

  if (unit.posts %in% c("distance", "time") == FALSE) {
    stop('If specified, unit.posts must be "distance" or "time".')
  } else {
    if (is.null(unit.interval)) {
      if (unit.posts == "distance")  {
        unit.interval <- 50
      } else if (unit.posts == "time") {
        unit.interval <- 60
      }
    } else {
      if (!is.numeric(unit.interval)) {
        stop('unit.interval must be numeric.')
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
      cumulative <- cholera::distanceTime(cumsum(edge.data$d),
        speed = x$speed)
    }

    total <- cumulative[length(cumulative)]
    posts <- seq(0, total, unit.interval)

    if (max(posts) > max(cumulative)) {
      posts <- posts[-length(posts)]
    }

    bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                       hi = cumulative)

    edge.select <- vapply(posts, function(x) {
      which(vapply(seq_len(nrow(bins)), function(i) {
        x >= bins[i, "lo"] & x < bins[i, "hi"]
      }, logical(1L)))
    }, integer(1L))

    start.node <- edgeOrder(edge.data, path.edge)

    post.coordinates <- lapply(seq_along(edge.select), function(i) {
      sel.data <- edge.data[edge.select[i], ]

      if (start.node[edge.select[i]] == 1) {
        e.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                             y = c(sel.data$y1, sel.data$y2))
      } else if (start.node[edge.select[i]] == 2) {
        e.data <- data.frame(x = c(sel.data$x2, sel.data$x1),
                             y = c(sel.data$y2, sel.data$y1))
      }

      ols <- stats::lm(y ~ x, data = e.data)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- atan(edge.slope)

      if (unit.posts == "distance") {
        h <- (posts[i] - bins[edge.select[i], "lo"]) / cholera::unitMeter(1)
      } else if (unit.posts == "time") {
        h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 * x$speed / 60^2 /
          cholera::unitMeter(1)
      }

      p.coords <- quandrantCoordinates(e.data, h, theta)

      data.frame(post = posts[i],
                 x = p.coords$x,
                 y = p.coords$y,
                 angle = theta * 180L / pi,
                 row.names = NULL)
    })

    coords <- do.call(rbind, post.coordinates)
    arrow.data <- edge.data[edge.select, ]
    start <- start.node[edge.select]

    invisible(lapply(seq_len(nrow(arrow.data)), function(i) {
      if (start[i] == 1) {
        dataB <- data.frame(x = c(arrow.data[i, "x2"], coords[i, "x"]),
                            y = c(arrow.data[i, "y2"], coords[i, "y"]))
      } else if (start[i] == 2) {
        dataB <- data.frame(x = c(arrow.data[i, "x1"], coords[i, "x"]),
                            y = c(arrow.data[i, "y1"], coords[i, "y"]))
      }

      zero.length.x <- round(abs(dataB[1, "x"] - dataB[2, "x"]), 2) == 0
      zero.length.y <- round(abs(dataB[1, "y"] - dataB[2, "y"]), 2) == 0

      if (any(zero.length.x | zero.length.y)) {
        text(dataB[1, c("x", "y")], labels = ">", srt = coords[i, "angle"],
          col = case.color, cex = 1.5)
      } else {
        arrows(dataB[1, "x"], dataB[1, "y"],
               dataB[2, "x"], dataB[2, "y"],
               lwd = 3, length = 0.075, col = case.color, code = 2)
      }
    }))
  }
}
