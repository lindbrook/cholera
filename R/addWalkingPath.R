#' Add the shortest walking path between a selected cases or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is \code{NULL}: this returns closest pump or "anchor" case. Character landmark name (case insensitive).
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of nodes.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the Vestry Report. \code{FALSE} uses the 13 in the original map.
#' @param distance.unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. \code{unit} is meaningful only when "weighted" is \code{TRUE}. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param unit.posts Character. "distance" for mileposts; "time" for timeposts.
#' @param unit.interval Numeric. Sets interval between posts: for "distance", the default is 50 meters; for "time", the default is 60 seconds.
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @note The function uses a case's "address" (i.e., a stack's "anchor" case) to compute distance. Time is computed using cholera::distanceTime(). Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network; they form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of \code{Inf}. Arrow points represent mileposts or timeposts to the destination.
#' @return An R list with two elements: a character vector of path nodes and a data frame summary.
#' @export
#' @examples
#' streetNameLocator("broad street", zoom = TRUE, highlight = FALSE,
#'   add.subtitle = FALSE)
#' addWalkingPath(447)

addWalkingPath <- function(origin = 1, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, unit.posts = "distance",
  unit.interval = NULL, alpha.level = 1) {

  arguments <- list(origin = origin,
                    destination = destination,
                    type = type,
                    observed = observed,
                    weighted = weighted,
                    vestry = vestry,
                    distance.unit = distance.unit,
                    time.unit = time.unit,
                    walking.speed = walking.speed)

  x <- do.call("walkingPath", arguments)

  if (is.na(x$alter.node)) {
    txt1 <- paste("Case", x$origin, "is part of an isolated subgraph.")
    txt2 <- "It (technically) has no nearest pump."
    stop(txt1, txt2)
  }

  if ((alpha.level > 0 & alpha.level <= 1) == FALSE) {
    stop('alpha.level must be > 0 and <= 1.')
  }

  colors <- snowColors(x$vestry)
  nodes <- x$nodes
  edges <- x$edges
  g <- x$g
  ego.node <- x$ego.node
  alter.node <- x$alter.node

  dat <- numericNodeCoordinates(x$path)

  if (is.null(x$origin) == FALSE) {
    if (grepl("Square", x$origin)) {
      if (x$origin == "Soho Square") {
        sq.center.origin <- data.frame(x = 18.07044, y = 15.85703)
      } else if (x$origin == "Golden Square") {
        sq.center.origin <- data.frame(x = 11.90927, y = 8.239483)
      }
    }
  }

  if (is.null(x$destination) == FALSE) {
    if (grepl("Square", x$destination)) {
      if (x$destination == "Soho Square") {
        sq.center.destination <- data.frame(x = 18.07044, y = 15.85703)
      } else if (x$destination == "Golden Square") {
        sq.center.destination <- data.frame(x = 11.90927, y = 8.239483)
      }
    }
  }

  if (x$type == "case-pump") {
    alter <- nodes[nodes$node == alter.node & nodes$pump != 0, "pump"]

    if (alpha.level != 1) {
      case.color <- grDevices::adjustcolor(colors[alter], alpha.f = alpha.level)
    } else {
      case.color <- colors[alter]
    }

    if (x$observed) {
      if (is.numeric(x$origin)) {
        origin.obs <- cholera::fatalities[cholera::fatalities$case == x$origin,
          c("x", "y")]
      } else if (is.character(x$origin)) {
        sel <- nodes$node == x$ego.node & nodes$anchor != 0
        nm <- cholera::landmarks[cholera::landmarks$case ==
          nodes[sel, "anchor"], "name"]
        origin.obs <- cholera::landmarks[cholera::landmarks$name == nm,
          c("x.proj", "y.proj")]
        names(origin.obs) <- c("x", "y")
      } else {
        origin.obs <- cholera::landmarks[cholera::landmarks$name == x$origin,
          c("x.proj", "y.proj")]
        names(origin.obs) <- c("x", "y")
      }
    } else {
      origin.obs <- cholera::regular.cases[x$origin, ]
    }

  } else if (x$type == "cases") {
    alter <- nodes[nodes$node == alter.node & nodes$anchor != 0, "anchor"]
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
    if (x$vestry) {
      origin.obs <- cholera::pumps.vestry[cholera::pumps.vestry$id ==
        x$origin, c("x", "y")]
    } else {
      origin.obs <- cholera::pumps[cholera::pumps$id == x$origin, c("x", "y")]
    }

    alter <- nodes[nodes$node == alter.node & nodes$anchor != 0, "anchor"]
    case.color <- "blue"
  }

  if (is.null(x$origin) == FALSE) {
    if (grepl("Square", x$origin)) {
      sel <- grep(x$origin, cholera::landmarks$name)
      sq.origin <- cholera::landmarks[sel, c("x.proj", "y.proj")]
      names(sq.origin) <- c("x", "y")
    }
  }

  if (is.null(x$destination) == FALSE) {
    if (grepl("Square", x$destination)) {
      sel <- grep(x$destination, cholera::landmarks$name)
      sq.destination <- cholera::landmarks[sel, c("x.proj", "y.proj")]
      names(sq.destination) <- c("x", "y")
    }
  }

  if (x$vestry) {
    pump.names <- paste0("p", cholera::pumps.vestry$id)
    points(cholera::pumps.vestry[, c("x", "y")], pch = 24, cex = 1,
      col = colors)
    text(cholera::pumps.vestry[, c("x", "y")], label = pump.names, pos = 1)
  } else {
    pump.names <- paste0("p", cholera::pumps$id)
    points(cholera::pumps[, c("x", "y")], pch = 24, cex = 1, col = colors)
    text(cholera::pumps[, c("x", "y")], label = pump.names, pos = 1)
  }

  if (x$type == "case-pump" | x$type == "cases") {
    points(origin.obs, col = "red")
  }

  points(dat[1, c("x", "y")], col = case.color, pch = 0)
  points(dat[nrow(dat), c("x", "y")], col = case.color, pch = 0)

  if (x$type %in% c("case-pump", "cases")) {
    if (x$observed) {
      if (is.numeric(x$origin)) {
        text(cholera::fatalities[cholera::fatalities$case == x$origin,
          c("x", "y")], labels = x$origin, pos = 1, col = "red")
      } else if (is.character(x$origin)) {
        if (x$origin == "Soho Square") {
          text(sq.center.origin$x, sq.center.origin$y,
            labels = "Soho\nSquare", col = "red", cex = 0.8)
        } else if (x$origin == "Golden Square") {
          text(sq.center.origin$x, sq.center.origin$y,
            labels = "Golden\nSquare", col = "red", cex = 0.8)
        } else {
          text(cholera::landmarks[cholera::landmarks$name == x$origin,
            c("x.proj", "y.proj")], labels = x$origin, pos = 1, col = "red")
        }
      }

      if (is.null(x$destination) == FALSE) {
        if (is.character(x$destination)) {
          if (x$destination == "Soho Square") {
            text(sq.center.destination$x, sq.center.destination$y,
              labels = "Soho\nSquare", col = "red", cex = 0.8)
          } else if (x$destination == "Golden Square") {
            text(sq.center.destination$x, sq.center.destination$y,
              labels = "Golden\nSquare", col = "red", cex = 0.8)
          } else {
            text(cholera::landmarks[cholera::landmarks$name == x$destination,
              c("x.proj", "y.proj")], labels = x$destination, pos = 1,
              col = "red")
          }
        }
      }

      if (x$type == "cases") {
        if (is.null(x$destination) | is.numeric(x$destination)) {
          points(cholera::fatalities[cholera::fatalities$case == x$data$caseB,
            c("x", "y")], col = "red")
          text(cholera::fatalities[cholera::fatalities$case == x$data$caseB,
            c("x", "y")], labels = x$data$caseB, pos = 1, col = "red")
        } else if (is.character(x$destination)) {
          if (grepl("Square", x$destination) == FALSE) {
            text(cholera::landmarks[cholera::landmarks$case == x$data$anchorB,
              c("x.proj", "y.proj")], labels = x$data$caseB, pos = 1,
              col = "red")
          }
        }

        if (is.null(x$origin) | is.numeric(x$origin)) {
          points(cholera::fatalities[cholera::fatalities$case == x$data$caseA,
            c("x", "y")], col = "red")
          text(cholera::fatalities[cholera::fatalities$case == x$data$caseA,
            c("x", "y")], labels = x$data$caseA, pos = 1, col = "red")
        } else if (is.character(x$origin)) {
          if (grepl("Square", x$origin) == FALSE) {
            text(cholera::landmarks[cholera::landmarks$case == x$data$anchorB,
              c("x.proj", "y.proj")], labels = x$data$caseA, pos = 1,
              col = "red")
          }
        }
      }
    } else {
      if (is.numeric(x$origin)) {
        text(cholera::regular.cases[x$origin, ], labels = x$origin, pos = 1,
          col = "red")
      } else if (is.character(x$origin)) {
        text(cholera::landmarks[cholera::landmarks$name == x$origin,
          c("x.proj", "y.proj")], labels = x$origin, pos = 1, col = "red")
      }

      if (x$type == "cases") {
        if (is.numeric(x$destination)) {
          text(cholera::regular.cases[x$data$caseB, ], labels = x$data$caseB,
            pos = 1, col = "red")
        } else if (is.character(x$origin)) {
          text(cholera::landmarks[cholera::landmarks$name == x$origin,
            c("x.proj", "y.proj")], labels = x$origin, pos = 1, col = "red")
        }
      }
    }
  }

  drawPath(x$path, case.color)

  d.unit <- distanceUnit(x$distance.unit)
  nominal.time <- nominalTime(x$data$time, x$time.unit)
  
  # mileposts #

  if (unit.posts %in% c("distance", "time") == FALSE) {
    stop('If specified, unit.posts must be "distance" or "time".')
  } else {
    if (is.null(unit.interval)) {
      if (unit.posts == "distance")  {
        unit.interval <- 50 * x$walking.speed / 5
      } else if (unit.posts == "time") {
        unit.interval <- 60 * x$walking.speed / 5
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
      cumulative <- unitMeter(cumsum(edge.data$d))
    } else if (unit.posts == "time") {
      cumulative <- distanceTime(cumsum(edge.data$d),
        walking.speed = x$walking.speed)
    }

    total <- cumulative[length(cumulative)]
    posts <- seq(0, total, unit.interval)

    if (max(posts) > total) posts <- posts[-length(posts)]

    bins <- data.frame(lo = c(0, cumulative[-length(cumulative)]),
                       hi = cumulative)

    edge.select <- vapply(posts, function(x) {
      which(vapply(seq_len(nrow(bins)), function(i) {
        x >= bins[i, "lo"] & x < bins[i, "hi"]
      }, logical(1L)))
    }, integer(1L))

    post.nodes <- path.edge[edge.select, ]

    post.coordinates <- lapply(seq_along(edge.select), function(i) {
      node1.node2 <-
      edge.data[edge.select[i], "node1"] == post.nodes[i, "node1"] &
      edge.data[edge.select[i], "node2"] == post.nodes[i, "node2"]

      node2.node1 <-
      edge.data[edge.select[i], "node1"] == post.nodes[i, "node2"] &
      edge.data[edge.select[i], "node2"] == post.nodes[i, "node1"]

      sel.data <- edge.data[edge.select[i], ]

      if (any(node1.node2)) {
        e.data <- data.frame(x = c(sel.data$x1, sel.data$x2),
                             y = c(sel.data$y1, sel.data$y2))
      } else if (any(node2.node1)) {
        e.data <- data.frame(x = c(sel.data$x2, sel.data$x1),
                             y = c(sel.data$y2, sel.data$y1))
      } else stop("Post error.")

      ols <- stats::lm(y ~ x, data = e.data)
      edge.slope <- stats::coef(ols)[2]
      edge.intercept <- stats::coef(ols)[1]
      theta <- atan(edge.slope)

      if (unit.posts == "distance") {
        h <- (posts[i] - bins[edge.select[i], "lo"]) / unitMeter(1)
      } else if (unit.posts == "time") {
        h <- (posts[i] - bins[edge.select[i], "lo"]) * 1000 * x$walking.speed /
          60^2 / unitMeter(1)
      }

      p.coords <- quandrantCoordinates(e.data, h, theta)

      data.frame(post = i,
                 x = p.coords$x,
                 y = p.coords$y,
                 angle = theta * 180L / pi,
                 start = ifelse(node1.node2, 1, ifelse(node2.node1, 2, 0)),
                 row.names = NULL)
    })

    coords <- do.call(rbind, post.coordinates)
    arrow.data <- edge.data[edge.select, ]
    start <- coords$start

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
        if (start[i] == 1) {
          text(dataB[1, c("x", "y")], labels = "<", srt = coords[i, "angle"],
            col = case.color, cex = 1.25)
        } else if (start[i] == 2) {
          text(dataB[1, c("x", "y")], labels = ">", srt = coords[i, "angle"],
            col = case.color, cex = 1.25)
        } else stop("Draw error.")
      } else {
        arrows(dataB[1, "x"], dataB[1, "y"],
               dataB[2, "x"], dataB[2, "y"],
               lwd = 3, length = 0.075, col = case.color, code = 2)
      }
    }))
  }
}
