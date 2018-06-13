#' Compute the shortest walking distance between cases and/or pumps.
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
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using distanceTime(). Adam and Eve Court, and Falconberg Court and Falconberg Mews, are disconnected from the larger road network and form two isolated subgraphs. This has two consequences: first, only cases on Adam and Eve Court can reach pump 2 and those cases cannot reach any other pump; second, cases on Falconberg Court and Mews cannot reach any pump. Unreachable pumps will return distances of "Inf".
#' @return An R data frame.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' \dontrun{
#' # distance from case 1 to nearest pump.
#' walkingDistance(1)
#'
#' # distance from case 1 to pump 6.
#' walkingDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' walkingDistance(1, -7)
#'
#' # distance from case 1 to case 6.
#' walkingDistance(1, 6, type = "cases")
#'
#' # distance from pump 1 to pump 6.
#' walkingDistance(1, 6, type = "pumps")
#' }

walkingDistance <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, weighted = TRUE, vestry = FALSE, unit = "meter",
  time.unit = "second", walking.speed = 5) {

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('"time.unit" must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (observed) {
    node.data <- cholera::neighborhoodData(vestry)
  } else {
    node.data <- cholera::neighborhoodData(vestry, case.set = "expected")
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g
  exp.ct <- nrow(cholera::regular.cases)

  if (type == "case-pump") {
    if (observed) {
      if (origin %in% 1:578 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
        txt2 <- '"origin" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (origin %in% 1:exp.ct == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- paste('"origin" must be between 1 and', paste0(exp.ct, "."))
        stop(paste(txt1, txt2))
      }
    }

    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(destination) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = TRUE",'
          txt2 <- '1 >= |destination| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          pumps <- cholera::pumps.vestry[destination, ]
        }
      } else {
        if (any(abs(destination) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = FALSE",'
          txt2 <- '1 >= |destination| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          pumps <- cholera::pumps[destination, ]
        }
      }
    } else {
      if (vestry) {
        pumps <- cholera::pumps.vestry
      } else {
        pumps <- cholera::pumps
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    } else {
      ego.id <- origin
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    }

    if (!is.null(destination)) {
      if (all(destination < 0)) {
        p.nodes <- nodes[nodes$pump != 0, ]
        alters <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE, "node"]
      } else {
        alters <- nodes[nodes$pump %in% destination, "node"]
      }
    } else {
      alters <- nodes[nodes$pump != 0, "node"]
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    if (all(is.infinite(d))) {
      sel <- which.min(d)
      alter.id <- NA
      p.name <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      alter.id <- nodes[nodes$node %in% names(sel), "pump"]
      p.name <- pumps[pumps$id == alter.id, "street"]
      alter.node <- names(sel)
    }

    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alter.id,
                      pump.name = p.name,
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)

  } else if (type == "cases") {
    if (observed) {
      if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be a whole number between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:exp.ct == FALSE)) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- 'both "origin" and "destination" must be whole numbers between'
        txt3 <- paste('1 and', paste0(exp.ct, "."))
        stop(paste(txt1, txt2, txt3))
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    } else {
      ego.id <- origin
      ego.node <- nodes[nodes$anchor == ego.id, "node"]
    }

    if (is.null(destination)) {
      alters <- nodes[nodes$anchor != 0 & nodes$node != ego.node, "node"]
    } else {
      if (observed) {
        if (all(destination > 0)) {
          alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% destination, "anchor.case"])
        } else if (all(destination < 0)) {
          alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% abs(destination) == FALSE, "anchor.case"])
        }
      } else {
        if (all(destination > 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination]
        } else if (all(destination < 0)) {
          alter.case <- nodes$anchor[nodes$anchor %in% destination == FALSE]
        }
      }

      alters <- nodes$node[nodes$anchor %in% alter.case &
                           nodes$node != ego.node]
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    if (all(is.infinite(d))) {
      alter.id <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      alter.id <- nodes[nodes$node %in% names(sel), "anchor"]
      alter.node <- names(sel)
    }

    if (is.null(destination) | all(destination < 0)) {
      out <- data.frame(caseA = origin,
                        caseB = alter.id,
                        anchorA = ego.id,
                        anchorB = alter.id,
                        distance = d[which.min(d)],
                        stringsAsFactors = FALSE,
                        row.names = NULL)
    } else if (all(destination > 0)) {
      if (length(destination) == 1) {
        out <- data.frame(caseA = origin,
                          caseB = destination,
                          anchorA = ego.id,
                          anchorB = alter.id,
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      } else if (length(destination) > 1) {
        out <- data.frame(caseA = origin,
                          caseB = destination[sel],
                          anchorA = ego.id,
                          anchorB = alter.id,
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE,
                          row.names = NULL)
      }
    }

  } else if (type == "pumps") {
    if (vestry) {
      pumps <- cholera::pumps.vestry

      if (any(abs(c(origin, destination)) %in% 1:14 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = TRUE",'
        txt2 <- 'origin and destination must whole numbers 1 >= |x| <= 14.'
        stop(paste(txt1, txt2))
      }
    } else {
      pumps <- cholera::pumps

      if (any(abs(c(origin, destination)) %in% 1:13 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = FALSE",'
        txt2 <- 'origin and destination must be whole numbers 1 >= |x| <= 13.'
        stop(paste(txt1, txt2))
      }
    }

    ego.node <- nodes[nodes$pump == origin, "node"]
    p.nodes <- nodes[nodes$pump > 0, ]

    if (is.null(destination)) {
      alters  <- p.nodes[p.nodes$pump != origin, "node"]
    } else {
      if (all(destination > 0)) {
        alters  <- p.nodes[p.nodes$pump %in% destination &
                           p.nodes$pump != origin, "node"]
      } else if (all(destination < 0)) {
        alters  <- p.nodes[p.nodes$pump %in% abs(destination) == FALSE &
                           p.nodes$pump != origin, "node"]
      }
    }

    if (weighted) {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x, weights = edges$d)
      }, numeric(1L))
    } else {
      d <- vapply(alters, function(x) {
        igraph::distances(g, ego.node, x)
      }, numeric(1L))
    }

    A <- p.nodes[p.nodes$node == ego.node, "pump"]
    ego.node <- p.nodes[p.nodes$node == ego.node, "node"]

    if (all(is.infinite(d))) {
      B <- NA
      alter.node <- NA
    } else {
      sel <- which.min(d)
      B <- p.nodes[p.nodes$node == names(sel), "pump"]
      alter.node <- p.nodes[p.nodes$node == names(sel), "node"]
    }

    out <- data.frame(pumpA = A,
                      nameA = pumps[pumps$id == A, "street"],
                      pumpB = B,
                      nameB = pumps[pumps$id == B, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)
  }

  out$time <- cholera::distanceTime(out$distance, unit = time.unit,
    speed = walking.speed)

  if (unit == "meter") {
    out$distance <- cholera::unitMeter(out$distance, "meter")
  } else if (unit == "yard") {
    out$distance <- cholera::unitMeter(out$distance, "yard")
  } else if (unit == "native") {
    out$distance <- cholera::unitMeter(out$distance, "native")
  }

  out
}
