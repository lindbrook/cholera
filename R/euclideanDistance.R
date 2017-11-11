#' Compute Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID of case or pump. Negative selection (exlusion) is possible with negative values. Default is NULL: this returns closest pump or case (in a different stack).
#' @param type Character "case-pump", "cases" or "pumps".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @note For "cases", "origin" and "destination" need to be a number between 1 and 578. To compute distance, the function uses a case's "address" (i.e., its "anchor case"). For "pumps", "origin" and "destination" must be numbers between 1 and 14 for vestry = TRUE, and 1 and 13 for vestry = FALSE.
#' @return An R vector.
#' @export
#' @examples
#' euclideanDistance(1, 2)
#' euclideanDistance(1, -7)
#' euclideanDistance(1, 2, type = "pumps")
#' euclideanDistance(1, 2, type = "cases")
#'
#' ## Pairwise Euclidean distance between pumps. ##
#' # pairs <- combn(cholera::pumps$id, 2, simplify = FALSE)
#' #
#' # vapply(pairs, function(x) {
#' #   euclideanDistance(x[1], x[2])$distance
#' # }, numeric(1L))

euclideanDistance <- function(origin, destination = NULL, type = "case-pump",
  vestry = FALSE, unit = NULL) {

  if (is.null(unit) == FALSE) {
    if (unit %in% c("meter", "yard") == FALSE)
      stop('If specified, "unit" must either be "meter" or "yard".')
  }

  if (type %in% c("case-pump", "cases", "pumps") == FALSE) {
    stop('"type" must be "case-pump", "cases" or "pumps".')
  }

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

    id <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    c.data <- cholera::fatalities[cholera::fatalities$case == id, c("x", "y")]

    d <- vapply(alters$id, function(i) {
      c(stats::dist(rbind(alters[alters$id == i, c("x", "y")], c.data)))
    }, numeric(1L))

    sel <- which.min(d)
    out <- data.frame(case = origin,
                      pump = alters[sel, "id"],
                      pump.name = alters[sel, "street"],
                      distance = d[sel],
                      stringsAsFactors = FALSE)

  } else if (type == "cases") {
    if (any(c(origin, destination) %in% 1:578 == FALSE)) {
      stop('With type = "cases", "origin" and "destination" must be between 1 and 578.')
    }

    ego.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
      origin, "anchor.case"])
    ego <- cholera::fatalities[cholera::fatalities$case == ego.id, ]

    if (is.null(destination)) {
      alters.id <- cholera::fatalities.address$anchor.case
    } else {
      if (all(destination > 0)) {
        alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          destination, "anchor.case"])
      } else if (all(destination < 0)) {
        alters.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          abs(destination) == FALSE, "anchor.case"])
      } else if (any(destination > 0) & any(destination < 0)) {
        pos <- destination[destination > 0]
        neg <- destination[destination < 0]
        pos.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          pos, "anchor.case"])
        neg.id <- unique(cholera::anchor.case[cholera::anchor.case$case %in%
          abs(neg) == FALSE, "anchor.case"])
        alters.id <- unique(c(pos.id, neg.id))
      }
    }

    if (identical(all.equal(ego.id, alters.id), TRUE)) {
      out <- data.frame(caseA = origin,
                        caseB = destination,
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
      out <- data.frame(caseA = ego$case,
                        caseB = alters$case[sel],
                        distance = d[which.min(d)],
                        stringsAsFactors = FALSE)
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

  if (!is.null(unit)) {
    if (unit == "yard") {
      out$distance <- out$distance * 177 / 3
    } else if (unit == "meter") {
      out$distance <- out$distance * 54
    }
  }

  out
}
