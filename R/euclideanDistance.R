#' Compute the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param observed Logical. Use observed or "simulated" expected data.
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param unit Character. Unit of distance: "meter", "yard" or "native". "native" returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Default is 5 km/hr.
#' @note The function uses a case's "address" (i.e., "anchor" case of a stack) to compute distance. Time is computed using distanceTime().
#' @return An R data frame.
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' euclideanDistance(1)
#'
#' # path from case 1 to pump 6.
#' euclideanDistance(1, 6)
#'
#' # exclude pump 7 from consideration.
#' euclideanDistance(1, -7)
#'
#' # path from case 1 to case 6.
#' euclideanDistance(1, 6, type = "cases")
#'
#' # path from pump 1 to pump 6.
#' euclideanDistance(1, 6, type = "pumps")

euclideanDistance <- function(origin, destination = NULL, type = "case-pump",
  observed = TRUE, vestry = FALSE, unit = "meter", time.unit = "second",
  walking.speed = 5) {

  n.sim.obs <- nrow(cholera::regular.cases)

  if (unit %in% c("meter", "yard", "native") == FALSE) {
    stop('"unit" must be "meter", "yard" or "native".')
  }

  if (time.unit %in% c("hour", "minute", "second") == FALSE) {
    stop('"time.unit" must be "hour", "minute" or "second".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

  if (type == "case-pump") {
    if (observed) {
      if (origin %in% 1:578 == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = TRUE,'
        txt2 <- '"origin" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (origin %in% 1:n.sim.obs == FALSE) {
        txt1 <- 'With type = "case-pump" and "observed" = FALSE,'
        txt2 <- '"origin" must be between 1 and'
        stop(paste(txt1, txt2, n.sim.obs, "."))
      }
    }

    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(destination) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = TRUE",'
          txt2 <- '1 >= |destination| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          alters <- cholera::pumps.vestry[destination, ]
        }
      } else {
        if (any(abs(destination) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "case-pump" and "vestry = FALSE",'
          txt2 <- '1 >= |destination| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          alters <- cholera::pumps[destination, ]
        }
      }
    } else {
      if (vestry) {
        alters <- cholera::pumps.vestry
      } else {
        alters <- cholera::pumps
      }
    }

    if (observed) {
      ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
        "anchor.case"]
      ego <- cholera::fatalities[cholera::fatalities$case == ego.id,
        c("x", "y")]
    } else {
      ego <- cholera::regular.cases[origin, ]
      ego.id <- as.numeric(row.names(ego))
    }

    d <- vapply(alters$id, function(i) {
      c(stats::dist(rbind(alters[alters$id == i, c("x", "y")], ego)))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(case = origin,
                      anchor = ego.id,
                      pump = alters[sel, "id"],
                      pump.name = alters[sel, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE)

  } else if (type == "cases") {
    if (observed) {
      if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be between 1 and 578.'
        stop(paste(txt1, txt2))
      }
    } else {
      if (any(abs(c(origin, destination)) %in% 1:n.sim.obs == FALSE)) {
        txt1 <- 'With type = "cases", the absolute value of both "origin"'
        txt2 <- 'and "destination" must be between 1 and'
        stop(paste(txt1, txt2, n.sim.obs, "."))
      }
    }

    if (observed) {
      ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
        origin, "anchor.case"])
      ego <- cholera::fatalities[cholera::fatalities$case == ego.id, ]
    } else {
      ego <- cholera::regular.cases[origin, ]
      ego.id <- as.numeric(row.names(ego))
    }

    if (observed) {
      if (is.null(destination)) {
        alters.id <- cholera::fatalities.address$anchor.case
      } else {
        if (all(destination > 0)) {
          alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% destination, "anchor.case"])
        } else if (all(destination < 0)) {
          alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case
            %in% abs(destination) == FALSE, "anchor.case"])
        }
      }
    } else {
      if (is.null(destination)) {
        alters.id <- cholera::sim.ortho.proj$case
      } else {
        if (all(destination > 0)) {
          alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
            destination, "case"]
        } else if (all(destination < 0)) {
          alters.id <- cholera::sim.ortho.proj[cholera::sim.ortho.proj$case %in%
            abs(destination) == FALSE, "case"]
        }
      }
    }

    if (identical(all.equal(ego.id, alters.id), TRUE)) {
      out <- data.frame(caseA = origin,
                        caseB = destination,
                        anchorA = ego.id,
                        anchorB = alters.id,
                        distance = 0,
                        stringsAsFactors = FALSE)
    } else {
      alters <- cholera::fatalities[cholera::fatalities$case %in% alters.id, ]
      alters <- alters[alters$case != ego.id, ]

      d <- vapply(alters$case, function(i) {
        dat <- rbind(ego[, c("x", "y")], alters[alters$case == i, c("x", "y")])
        c(stats::dist(dat))
      }, numeric(1L))

      sel <- which.min(d)

      if (is.null(destination) | all(destination < 0)) {
        out <- data.frame(caseA = origin,
                          caseB = alters$case[sel],
                          anchorA = ego$case,
                          anchorB = alters$case[sel],
                          distance = d[which.min(d)],
                          stringsAsFactors = FALSE)
      } else if (all(destination > 0)) {
        if (length(destination) == 1) {
          out <- data.frame(caseA = origin,
                            caseB = destination,
                            anchorA = ego$case,
                            anchorB = alters$case[sel],
                            distance = d[which.min(d)],
                            stringsAsFactors = FALSE)
        } else if (length(destination) > 1) {
          out <- data.frame(caseA = origin,
                            caseB = destination[sel],
                            anchorA = ego$case,
                            anchorB = alters$case[sel],
                            distance = d[which.min(d)],
                            stringsAsFactors = FALSE)
        }
      }
    }

  } else if (type == "pumps") {
    if (!is.null(destination)) {
      if (vestry) {
        if (any(abs(c(origin, destination)) %in% 1:14 == FALSE)) {
          txt1 <- 'With type = "pumps" and "vestry = TRUE",'
          txt2 <- 'origin and destination must be 1 >= |x| <= 14.'
          stop(paste(txt1, txt2))
        } else {
          ego <- cholera::pumps.vestry[cholera::pumps.vestry$id == origin, ]
          alters <- cholera::pumps.vestry[destination, ]
          alters <- alters[alters$id != origin, ]
        }
      } else {
        if (any(abs(c(origin, destination)) %in% 1:13 == FALSE)) {
          txt1 <- 'With type = "pumps" and "vestry = FALSE",'
          txt2 <- 'origin and destination must be 1 >= |x| <= 13.'
          stop(paste(txt1, txt2))
        } else {
          ego <- cholera::pumps[cholera::pumps$id == origin, ]
          alters  <- cholera::pumps[destination, ]
          alters <- alters[alters$id != origin, ]
        }
      }
    } else {
      if (vestry) {
        ego <- cholera::pumps.vestry[cholera::pumps.vestry$id == origin, ]
        alters <- cholera::pumps.vestry[cholera::pumps.vestry$id != origin, ]
      } else {
        ego <- cholera::pumps[cholera::pumps$id == origin, ]
        alters <- cholera::pumps[cholera::pumps$id != origin, ]
      }
    }

    d <- vapply(alters$id, function(i) {
      dat <- rbind(ego[, c("x", "y")], alters[alters$id == i, c("x", "y")])
      c(stats::dist(dat))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(pumpA = ego$id,
                      pumpB = alters$id[sel],
                      pump.nameA = ego$street,
                      pump.nameB = alters$street[sel],
                      distance = d[which.min(d)],
                      stringsAsFactors = FALSE)
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
