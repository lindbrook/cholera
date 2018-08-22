#' Compute Pearson Residuals (prototype)
#'
#' @param x An object created by \code{neighborhoodEuclidean()}, \code{neighborhoodVoronoi()} or \code{neighborhoodWalking()}.
#' @seealso \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodVoronoi}}, \code{\link{neighborhoodEuclidean}},
#' @return An R vector.
#' @export
#' @examples
#' \dontrun{
#'
#' pearsonResiduals(neighborhoodEuclidean())
#' pearsonResiduals(neighborhoodVoronoi())
#' pearsonResiduals(neighborhoodWalking())
#' }

pearsonResiduals <- function(x) UseMethod("pearsonResiduals", x)

pearsonResiduals.default <- function(x) NULL

#' @export
pearsonResiduals.euclidean <- function(x) {
  obs <- unclass(table(x$nearest.pump))
  exp <- unclass(table(cholera::neighborhoodEuclidean(pump.select = x$pump.id,
    case.set = "expected", multi.core = x$cores)$nearest.pump))

  exp.data <- data.frame(pump.id = as.numeric(names(exp)),
                         exp.ct = exp,
                         Percent = round(100 * exp / sum(exp), 2))

  obs.data <- data.frame(pump.id = as.numeric(names(obs)),
                         Count = obs)

  output <- merge(obs.data, exp.data, by = "pump.id")
  output$exp.ct <- NULL
  output$Expected <- sum(output$Count) * output$Percent / 100
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.voronoi <- function(x) {
  census <- x$statistic.data
  count <- vapply(census, sum, numeric(1L))
  output <- data.frame(pump.id = as.numeric(names(count)),
                       Count = count,
                       Percent = round(100 * count / sum(count), 2))
  output <- merge(output, x$expected.data[, c("pump", "pct")],
    by.x = "pump.id", by.y = "pump")
  output$Expected <- output$pct * sum(output$Count)
  output$pct <- NULL
  output$Pearson <- pearson(output)
  output
}

#' @export
pearsonResiduals.walking <- function(x) {
  dat <- expectedWalkingLength(x)
  obs <- dat$obs
  exp <- dat$exp
  exp.data <- data.frame(pump.id = as.numeric(names(exp)),
                         exp.ct = exp,
                         Percent = round(100 * exp / sum(exp), 2))

  obs.data <- data.frame(pump.id = as.numeric(names(obs)),
                         Count = obs)

  output <- merge(obs.data, exp.data, by = "pump.id")
  output$exp.ct <- NULL
  output$Expected <- sum(output$Count) * output$Percent / 100
  output$Pearson <- pearson(output)
  output
}

pearson <- function(x) {
  (x$Count - x$Expected) / sqrt(x$Expected)
}

#' Compute Total Length of Roads by Neighborhood (prototype)
#'
#' @param x An object created by \code{neighborhoodWalking()}.
#' @export
expectedWalkingLength <- function(x) {
  if (class(x) != "walking") {
    stop('"x"\'s class needs to be "walking".')
  }

  dat <- cholera::neighborhoodData(vestry = x$vestry, case.set = "observed")
  edges <- dat$edges
  p.data <- dat$nodes.pump

  if (is.null(x$pump.select)) {
    p.node <- p.data$node
    p.name <- p.data$pump
  } else {
    if (all(x$pump.select > 0)) {
      p.data <- p.data[p.data$pump %in% x$pump.select, ]
    } else if (all(x$pump.select < 0)) {
      p.data <- p.data[p.data$pump %in% abs(x$pump.select) == FALSE, ]
    }
    p.node <- p.data$node
    p.name <- p.data$pump
  }

  n.path.edges <- parallel::mclapply(x$paths, function(neighborhood) {
    lapply(neighborhood, auditEdge, edges)
  }, mc.cores = x$cores)

  ##
  obs.segment.count <- lapply(n.path.edges, function(x) {
    table(edges[unique(unlist(x)), "id"])
  })

  edge.count <- table(edges$id)

  segment.audit <- lapply(obs.segment.count, function(neighborhood) {
    whole.id <- vapply(names(neighborhood), function(nm) {
      identical(neighborhood[nm], edge.count[nm])
    }, logical(1L))

    list(whole = names(neighborhood[whole.id]),
         partial = names(neighborhood[!whole.id]))
  })

  ## ------------ Observed ------------ ##

  # list of whole traversed segments
  obs.whole <- lapply(segment.audit, function(x) x$`whole`)

  # list of partially traversed segments
  obs.partial <- lapply(segment.audit, function(x) x$`partial`)
  partial.segs <- unname(unlist(obs.partial))
  obs.partial.whole <- wholeSegments(partial.segs, dat, edges, p.name,
    p.node, x)

  # list of of split segments (lead to different pumps)
  # the cutpoint is found using appox. 1 meter increments via cutpointValues()
  obs.partial.segments <- setdiff(partial.segs, unlist(obs.partial.whole))

  if (length(obs.partial.segments) > 0) {
    obs.partial.split.data <- parallel::mclapply(obs.partial.segments,
      splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    cutpoints <- cutpointValues(obs.partial.split.data)
    obs.partial.split.pump <- lapply(obs.partial.split.data, function(x)
      unique(x$pump))
    obs.partial.split <- splitData(obs.partial.segments, cutpoints, edges)
  }

  ## ------------ Unobserved ------------ ##

  # list of edges that are wholly or partially traversed
  obs.segments <- lapply(n.path.edges, function(x) {
    unique(edges[unique(unlist(x)), "id"])
  })

  # list of edges that are untouched by any path
  unobs.segments <- setdiff(cholera::road.segments$id, unlist(obs.segments))

  falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
  unobs.segments <- unobs.segments[unobs.segments %in%
    falconberg.ct.mews == FALSE]

  # Exclude segment if A&E pump is not among selected.
  if (is.null(x$pump.select) == FALSE) {
    sel <- "Adam and Eve Court"
    AE.pump <- cholera::pumps[cholera::pumps$street == sel, "id"]
    AE <- cholera::road.segments[cholera::road.segments$name == sel, "id"]

    if (all(x$pump.select > 0)) {
      if (AE.pump %in% x$pump.select == FALSE) {
        unobs.segments <- unobs.segments[unobs.segments %in% AE == FALSE]
      }
    } else if (all(x$pump < 0)) {
      if (AE.pump %in% abs(x$pump.select)) {
        unobs.segments <- unobs.segments[unobs.segments %in% AE == FALSE]
      }
    }
  }

  unobs.whole <- wholeSegments(unobs.segments, dat, edges, p.name, p.node, x)
  unobs.split.segments <- setdiff(unobs.segments, unlist(unobs.whole))

  if (length(unobs.split.segments) > 0) {
    unobs.split.data <- parallel::mclapply(unobs.split.segments,
      splitSegments, edges, p.name, p.node, x, mc.cores = x$cores)
    cutpoints <- cutpointValues(unobs.split.data)
    unobs.split.pump <- lapply(unobs.split.data, function(x) unique(x$pump))
    unobs.split <- splitData(unobs.split.segments, cutpoints, edges)
  }

  ## ------------ Data Assembly ------------ ##

  if (x$vestry) {
    pumpID <- seq_len(nrow(cholera::pumps.vestry))
  } else {
    pumpID <- seq_len(nrow(cholera::pumps))
  }

  wholes <- lapply(pumpID, function(nm) {
    c(obs.whole[[paste(nm)]],
      unobs.whole[[paste(nm)]],
      obs.partial.whole[[paste(nm)]])
  })

  names(wholes) <- pumpID

  total.whole.road.length <- lapply(wholes, function(x) {
    sum(vapply(x, segmentLength, numeric(1L)))
  })

  #

  # split segments #
  split.test1 <- length(obs.partial.segments)
  split.test2 <- length(unobs.split.segments)

  if (split.test1 > 0 & split.test2 == 0) {
    splits <- obs.partial.split
    splits.pump <- obs.partial.split.pump
    split.segs <- obs.partial.segments
  } else if (split.test1 == 0 & split.test2 > 0) {
    splits <- unobs.split
    splits.pump <- unobs.split.pump
    split.segs <- unobs.split.segments
  } else if (split.test1 > 0 & split.test2 > 0) {
    splits <- c(obs.partial.split, unobs.split)
    splits.pump <- c(obs.partial.split.pump, unobs.split.pump)
    split.segs <- c(obs.partial.segments, unobs.split.segments)
  }

  split.road.length <- lapply(seq_along(splits), function(i) {
    dat <- splits[[i]]
    lst <- list(rbind(dat[1, c("x", "y")], dat[2, c("x", "y")]),
                rbind(dat[3, c("x", "y")], dat[4, c("x", "y")]))
    data.frame(dist = vapply(lst, stats::dist, numeric(1L)),
               pump = splits.pump[[i]])
  })

  split.road.length <- do.call(rbind, split.road.length)
  split.pumps <- unique(split.road.length$pump)

  total.split.road.length <- lapply(split.pumps, function(p) {
     sum(split.road.length[split.road.length$pump == p, "dist"])
  })

  names(total.split.road.length) <- split.pumps

  vapply(names(total.whole.road.length), function(nm) {
    if (nm %in% names(total.split.road.length)) {
      total.whole.road.length[[nm]] + total.split.road.length[[nm]]
    } else {
      total.whole.road.length[[nm]]
    }
  }, numeric(1L))
}
