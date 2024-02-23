#' Compute Euclidean path coordinates from case/landmark to nearest/selected pump.
#'
#' @param origin Numeric. Vector of origin(s) (numeric ID or character name landmark/pump ).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.
#' @importFrom geosphere distGeo
#' @export

euclideanPathB <- function(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, include.landmarks = TRUE) {

  meter.to.yard <- 1.09361

  if (is.null(origin) & is.null(destination)) {
    stop("You must provide at least one origin or destination.", call. = FALSE)
  }

  if (!type %in% c("case-pump", "cases", "pumps")) {
    stop('type must be "case-pump", "cases" or "pumps".', call. = FALSE)
  }

  if (any(is.character(origin))) origin <- caseLandmarks(origin)
  if (any(is.character(destination))) destination <- caseLandmarks(destination)

  if (!include.landmarks & type %in% c("case-pump", "cases")) {
    msg <- 'landmarks not considered when include.landmarks = FALSE.'
    if (is.numeric(origin)) {
      if (origin > 1000L) stop(msg, call. = FALSE)
    } else if (is.character(origin)) {
      lndmrk.test <- origin %in% cholera::landmarksB$name |
                     origin %in% cholera::landmark.squaresB$name
      if (lndmrk.test) stop(msg, call. = FALSE)
    }
    if (is.numeric(destination)) {
      if (destination > 1000L) stop(msg, call. = FALSE)
    } else if (is.character(destination)) {
      lndmrk.test <- destination %in% cholera::landmarksB$name |
                     destination %in% cholera::landmark.squaresB$name
      if (lndmrk.test) stop(msg, call. = FALSE)
    }
  }

  # Change type to "cases" in presence of destination landmarks
  if (is.character(destination)) {
    dest.nm <- c(cholera::landmark.squaresB$name, cholera::landmarksB$name)
    if (any(destination %in% dest.nm) & type != "cases") type <- "cases"
  } else if (is.numeric(destination)) {
    dest.num <- c(cholera::landmark.squaresB$case, cholera::landmarksB$case)
    if (any(destination %in% dest.num) & type != "cases") type <- "cases"
  }

  if (vestry) pmp <- cholera::pumps.vestry
  else pmp <- cholera::pumps

  if (type == "case-pump") {
    origin.chk <- validateCase(origin, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "cases") {
    origin.chk <- validateCase(origin, include.landmarks)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validateCase(destination, include.landmarks)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm

  } else if (type == "pumps") {
    origin.chk <- validatePump(origin, pmp, vestry)
    orgn <- origin.chk$out
    orgn.nm <- origin.chk$out.nm

    destination.chk <- validatePump(destination, pmp, vestry)
    dstn <- destination.chk$out
    dstn.nm <- destination.chk$out.nm
  }

  if (type == "case-pump") {
    path.data <- casePumpEucl(orgn, orgn.nm, destination, dstn, latlong, pmp,
      vestry)
  } else if (type == "cases") {
    path.data <- caseCaseEucl(orgn, orgn.nm, destination, dstn,
      include.landmarks, latlong, origin, vestry)
  } else if (type == "pumps") {
    path.data <- pumpPumpEucl(orgn, orgn.nm, destination, dstn, latlong, origin, pmp, vestry)
  }

  if (length(orgn) > 1) orgn <- path.data$data$orgn
  if (length(orgn.nm) > 1) orgn.nm <- path.data$data$orgn.nm
  nearest.dest <- path.data$data$nearest.dest

  if (latlong) {
    walking.time <- walkingTime(path.data$data$d, time.unit = time.unit,
      walking.speed = walking.speed)
  } else {
    d.est <- unitMeter(path.data$data$d, distance.unit = distance.unit)
    walking.time <- distanceTime(d.est, distance.unit = distance.unit,
      time.unit = time.unit, walking.speed = walking.speed)
  }

  if (as.integer(nearest.dest) < 1000L) {
    if (type %in% c("case-pump", "pumps")) {
      dest.nm <- pmp[pmp$id == nearest.dest, ]$street
    } else if (type == "cases") {
      dest.nm <- nearest.dest
    }
  } else if (as.integer(nearest.dest) >= 1000L) {
    sel <- cholera::landmarksB$case == as.integer(nearest.dest)
    dest.nm <- cholera::landmarksB[sel, ]$name
    if (grepl("Square", dest.nm)) {
      sel <- cholera::landmarksB$case == nearest.dest
      tmp <- strsplit(cholera::landmarksB[sel, ]$name, "-")
      dest.nm <- unlist(tmp)[1]
    }
  }

  data.summary <- data.frame(orig = orgn,
                             dest = nearest.dest,
                             orig.nm = orgn.nm,
                             dest.nm = dest.nm,
                             distance = path.data$data$d,
                             time = walking.time,
                             type = type,
                             row.names = NULL)

  output <- list(# path = path,
    # ego.coords = ego.coords,
                 ego = path.data$ego,
                 alter = path.data$alter,
                 data = data.summary,
                 destination = destination,
                 vestry = vestry,
                 # ds = path.data$d,
                 distance.unit = distance.unit,
                 latlong = latlong,
                 # edges = edges,
                 pmp = pmp,
                 time.unit = time.unit,
                 walking.speed = walking.speed)

  class(output) <- "euclidean_path_B"
  output
}

#' Print method for walkingPathB().
#'
#' Summary output.
#' @param x An object of class "euclidean_path_B" created by euclideanPathB().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export

print.euclidean_path_B <- function(x, ...) {
  if (!inherits(x, "euclidean_path_B")) {
    stop('"x"\'s class must be "euclidean_path_B".')
  }
  print(x[c("ego", "alter", "data")])
}

casePumpEucl <- function(orgn, orgn.nm, destination, dstn, latlong, pmp, vestry) {
  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  fatal <- cholera::fatalities$case %in% orgn
  land <- cholera::landmarksB$case %in% orgn
  # sq <- cholera::landmark.squaresB$case %in% orgn  # via validateCase()!

  if (any(fatal) & any(land)) {
    a <- cholera::fatalities[cholera::fatalities$case %in% orgn, vars]
    b <- cholera::landmarksB[cholera::landmarksB$case %in% orgn, vars]
    ego.coords <- rbind(a, b)
  } else if (all(!fatal) & any(land)) {
    ego.coords <- cholera::landmarksB[cholera::landmarksB$case %in% orgn, vars]
  } else if (any(fatal) & all(!land)) {
    ego.coords <- cholera::fatalities[cholera::fatalities$case %in% orgn, vars]
  }

  alter.coords <- pmp[pmp$id %in% dstn, vars]

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[which.min(d), ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.pump, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

caseCaseEucl <- function(orgn, orgn.nm, destination, dstn, include.landmarks,
  latlong, origin, vestry) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  sq.cases <- sort(c(sqCases("Golden"), sqCases("Soho")))

  if (is.null(origin) & !is.null(destination)) {
    sq.destination <- (grepl("Square", destination) |
                       destination %in% sq.cases) &
                      is.null(origin)

    if (sq.destination) {
      if (is.character(orgn)) var <- "name"
      else if (is.numeric(orgn)) var <- "case"
      gold <- sqCases("Golden", var)
      soho <- sqCases("Soho", var)

       if (any(dstn %in% gold)) {
        sel <- !orgn %in% gold
        orgn <- orgn[sel]
        orgn.nm <- orgn.nm[sel]
      }

      if (any(dstn %in% soho)) {
        sel <- !orgn %in% soho
        orgn <- orgn[sel]
        orgn.nm <- orgn.nm[sel]
      }
    }

    if (any(dstn %in% orgn)) {
      sel <- !orgn %in% dstn
      orgn <- orgn[sel]
      orgn.nm <- orgn.nm[sel]
    }
  }

  # Origin (egos) #

  sel <- cholera::anchor.case$case %in% orgn
  anchor <- cholera::anchor.case[sel, ]$anchor
  fatal <- cholera::fatalities.address$anchor %in% anchor

  land <- cholera::landmarksB$case %in% orgn

  if (any(fatal) & any(land)) {
    a <- cholera::fatalities.address[fatal, vars]
    b <- cholera::landmarksB[land, vars]
    ego.coords <- rbind(a, b)
  } else if (all(!fatal) & any(land)) {
    ego.coords <- cholera::landmarksB[land, vars]
  } else if (any(fatal) & all(!land)) {
    ego.coords <- cholera::fatalities.address[fatal, vars]
  }

  if (!is.null(origin) & is.null(destination)) {
    sq.origin <- (grepl("Square", origin) |
                  origin %in% sq.cases) &
                 is.null(destination)

    if (sq.origin) {
      if (is.character(dstn)) var <- "name"
      else if (is.numeric(dstn)) var <- "case"
      gold <- sqCases("Golden", var)
      soho <- sqCases("Soho", var)
      if (any(orgn %in% gold)) dstn <- dstn[!dstn %in% gold]
      if (any(orgn %in% soho)) dstn <- dstn[!dstn %in% soho]
    }

    if (any(orgn %in% dstn)) {
      dstn <- dstn[dstn %in% cholera::fatalities.address$anchor]
      dstn <- dstn[!dstn %in% anchor]
    }
  }

  # Destination (alters) #

  fatal <- cholera::fatalities.address$anchor %in% dstn
  land <- cholera::landmarksB$case %in% dstn

  if (any(fatal) & any(land)) {
    a <- cholera::fatalities.address[fatal, vars]
    b <- cholera::landmarksB[land, vars]
    alter.coords <- rbind(a, b)
  } else if (all(!fatal) & any(land)) {
    alter.coords <- cholera::landmarksB[land, vars]
  } else if (any(fatal) & all(!land)) {
    alter.coords <- cholera::fatalities.address[fatal, vars]
  }

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    nearest.dest <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[which.min(d), ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.dest <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.dest, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

pumpPumpEucl <- function(orgn, orgn.nm, destination, dstn, latlong, origin, pmp,
  vestry) {

  if (latlong) vars <- c("lon", "lat")
  else vars <- c("x", "y")

  if (!is.null(origin) & is.null(destination)) {
    if (any(orgn %in% dstn)) dstn <- dstn[!dstn %in% orgn]
  }

  if (is.null(origin) & !is.null(destination)) {
    if (any(dstn %in% orgn)) orgn <- orgn[!orgn %in% dstn]
  }

  ego.coords <- pmp[pmp$id %in% orgn, vars]
  alter.coords <- pmp[pmp$id %in% dstn, vars]

  sel <- seq_len(nrow(alter.coords))

  if (nrow(ego.coords) == 1) {
    d <- stats::dist(rbind(ego.coords, alter.coords))[sel]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords
    alter <- alter.coords[which.min(d), ]
  } else if (length(ego.coords) > 1) {
    d.multi.ego <- lapply(seq_len(nrow(ego.coords)), function(i) {
      stats::dist(rbind(ego.coords[i, ], alter.coords))[sel]
    })
    ego.id <- which.min(vapply(d.multi.ego, min, numeric(1L)))
    orgn <- orgn[ego.id]
    orgn.nm <- orgn.nm[ego.id]
    d <- d.multi.ego[[ego.id]]
    nearest.pump <- dstn[which.min(d)]
    nearest.d <- min(d)

    ego <- ego.coords[ego.id]
    alter <- alter.coords[which.min(d), ]
  }

  data.summary <- data.frame(orgn = orgn, orgn.nm = orgn.nm,
    nearest.dest = nearest.pump, d = nearest.d)

  list(ego = ego, alter = alter, data = data.summary)
}

