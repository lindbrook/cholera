#' Compute the shortest walking distance between cases and/or pumps.
#'
#' Beta v.1
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param weighted Logical. TRUE computes shortest path in terms of road length. FALSE computes shortest path in terms of nodes.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. Meaningful only when "weighted" is TRUE. See \code{vignette("roads")} for information on unit distances.
#' @note The function uses a case's "address" or "anchor" case to compute distance. Because Adam and Eve Court is disconnected from the larger road network (an isolate), only cases on that road can reach pump 2. All others will return Inf.
#' @return A base R data frame.
#' @seealso \code{\link{fatalities}}, \code{vignette("pump.neighborhoods")}
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' walkingDistance(1)
#'
#' # path from case 1 to nearest pump in meters (appox).
#' walkingDistance(1, unit = "meter")
#'
#' # path from case 1 to pump 6.
#' walkingDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' walkingDistance(1, -7)
#'
#' # path from case 1 to case 6.
#' walkingDistance(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' walkingDistance(1, 6, type = "pumps")
#'
#' ## Pairwise Euclidean distance (meters) between pumps. ##
#' # pairs <- combn(cholera::pumps$id, 2, simplify = FALSE)
#' #
#' # vapply(pairs, function(x) {
#' #   walkingDistance(x[1], x[2], type = "pumps", unit = "meter")$distance
#'# }, numeric(1L))

walkingDistance <- function(origin, destination = NULL, type = "case-pump",
  weighted = TRUE, vestry = FALSE, unit = NULL) {

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (vestry) {
    node.data <- cholera::nodeData(vestry = TRUE)
  } else {
    node.data <- cholera::nodeData()
  }

  nodes <- node.data$nodes
  edges <- node.data$edges
  g <- node.data$g

  if (type == "case-pump") {
    if (origin %in% 1:578 == FALSE) {
      stop('With type = "case-pump", "origin" must be between 1 and 578.')
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

    ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    ego.node <- nodes[nodes$anchor == ego.id, "node"]

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

    sel <- which.min(d)
    alter.id <- nodes[nodes$node %in% names(sel), "pump"]

    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alter.id,
                      pump.name = pumps[pumps$id == alter.id, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)

  } else if (type == "cases") {
    if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
      txt1 <- 'With type = "cases", the absolute value of both "origin"'
      txt2 <- 'and "destination" must be between 1 and 578.'
      stop(paste(txt1, txt2))
    }

    ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    ego.node <- nodes[nodes$anchor == ego.id, "node"]

    if (is.null(destination)) {
      alters <- nodes[nodes$anchor != 0 & nodes$node != ego.node, "node"]
    } else {
      if (all(destination > 0)) {
        alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          destination, "anchor.case"])
      } else if (all(destination < 0)) {
        alter.case <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          abs(destination) == FALSE, "anchor.case"])
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

    sel <- which.min(d)
    alter.id <- nodes[nodes$node %in% names(sel), "anchor"]

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
      if (any(abs(c(origin, destination)) %in% 1:14 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = TRUE",'
        txt2 <- 'origin and destination must be 1 >= |x| <= 14.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:13 == FALSE)) {
        txt1 <- 'With type = "pumps" and "vestry = FALSE",'
        txt2 <- 'origin and destination must be 1 >= |x| <= 13.'
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

    sel <- which.min(d)
    A <- p.nodes[p.nodes$node == ego.node, "pump"]
    B <- p.nodes[p.nodes$node == names(sel), "pump"]

    out <- data.frame(pumpA = A,
                      nameA = pumps[pumps$id == A, "street"],
                      pumpB = B,
                      nameB = pumps[pumps$id == B, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE,
                      row.names = NULL)
  }

  if (!is.null(unit)) {
    if (unit == "yard") {
      out$distance <- out$distance * 177 / 3
    } else if (unit == "meter") {
      out$distance <- out$distance * 54
    }
  }

  out
}
