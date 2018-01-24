#' Compute the Euclidean distance between cases and/or pumps.
#'
#' @param origin Numeric or Integer. Numeric ID of case or pump.
#' @param destination Numeric or Integer. Numeric ID(s) of case(s) or pump(s). Exclusion is possible via negative selection (e.g., -7). Default is NULL: this returns closest pump or "anchor" case.
#' @param type Character "case-pump", "cases" or "pumps".
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 pumps from the original map.
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale. See \code{vignette("roads")} for information on unit distances.
#' @note The function uses a case's "address" or "anchor" case to compute distance.
#' @return An R list.
#' @export
#' @examples
#' # path from case 1 to nearest pump.
#' euclideanDistance(1)
#'
#' # path from case 1 to nearest pump in meters (approximate).
#' euclideanDistance(1, unit = "meter")
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
#'
#' # Plot result
#' plot(euclideanDistance(1, unit = "meter"))

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

    ego.id <- cholera::anchor.case[cholera::anchor.case$case == origin,
      "anchor.case"]
    ego <- cholera::fatalities[cholera::fatalities$case == ego.id, c("x", "y")]

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
    if (any(abs(c(origin, destination)) %in% 1:578 == FALSE)) {
      txt1 <- 'With type = "cases", the absolute value of both "origin"'
      txt2 <- 'and "destination" must be between 1 and 578.'
      stop(paste(txt1, txt2))
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

  if (!is.null(unit)) {
    if (unit == "meter") {
      out$distance <- cholera::unitMeter(out$distance, "meter")
    } else if (unit == "yard") {
      out$distance <- cholera::unitMeter(out$distance, "yard")
    }
  }

  output <- list(origin = origin, destination = destination, type = type,
    alters = alters, sel = sel, vestry = vestry, unit = unit, summary = out)

  class(output) <- "euclidean_distance"
  output
}


#' Summary of euclideanDistance().
#'
#' Print method for euclideanDistance()
#' @param x An object of class "euclidean_distance" created by euclideanDistance().
#' @param ... Additional parameters.
#' @return An R data frame.
#' @export
#' @examples
#' euclideanDistance(1)
#' print(euclideanDistance(1))

print.euclidean_distance <- function(x, ...) {
  if (class(x) != "euclidean_distance") {
    stop('"x"\'s class needs to be "euclidean_distance".')
  }

  print(x$summary)
}

#' Plot the Euclidean distance between cases and/or pumps.
#'
#' Plot method for euclideanDistance().
#' @param x An object of class "euclidean_distance" created by euclideanDistance().
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param ... Additional plotting parameters.
#' @return A base R plot.
#' @export
#' @examples
#' plot(euclideanDistance(1))

plot.euclidean_distance <- function(x, zoom = TRUE, radius = 0.5, ...) {
  if (class(x) != "euclidean_distance") {
    stop('"x"\'s class needs to be "euclidean_distance".')
  }

  rd <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  roads.list <- split(rd[, c("x", "y")], rd$street)
  border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  if (x$vestry) {
    colors <- cholera::snowColors(vestry = TRUE)
    pmp <- cholera::pumps.vestry
  } else {
    colors <- cholera::snowColors()
    pmp <- cholera::pumps
  }

  end.sel <- x$sel

  if (x$type == "case-pump" | x$type == "pumps") {
    end.pump <- x$alters$id[end.sel]
    alter.xy <- pmp[pmp$id == end.pump, c("x", "y")]
  } else if (x$type == "cases") {
    end.case <- x$alters$case[end.sel]
    alter.xy <- cholera::fatalities[cholera::fatalities$case == end.case,
      c("x", "y")]
  }

  if (x$type == "case-pump" | x$type == "cases") {
    origin.xy <- cholera::fatalities[cholera::fatalities$case == x$origin,
      c("x", "y")]
    ego.id <- cholera::anchor.case[cholera::anchor.case$case == x$origin,
      "anchor.case"]
    addr <- cholera::fatalities.address
    ego.xy <- addr[addr$anchor.case == ego.id,  c("x", "y")]
  } else if (x$type == "pumps") {
    origin.xy <- pmp[pmp$id == x$origin, c("x", "y")]
    ego.xy <- origin.xy
  }

  dat <- rbind(origin.xy, alter.xy)

  if (zoom) {
    x.rng <- c(min(dat$x) - radius, max(dat$x) + radius)
    y.rng <- c(min(dat$y) - radius, max(dat$y) + radius)
  } else {
    x.rng <- range(cholera::roads$x)
    y.rng <- range(cholera::roads$y)
  }

  plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
    xlab = "x", ylab = "y", pch = 15, cex = 0.5, col = "lightgray", asp = 1)
  invisible(lapply(roads.list, lines, col = "lightgray"))
  invisible(lapply(border.list, lines))

  if (x$type == "case-pump") {
    case.color <- colors[end.pump]
    points(origin.xy, col = "red")
    title(main = paste("Case", x$origin, "to Pump", end.sel))
  } else if (x$type == "cases") {
    case.color <- "blue"
    points(origin.xy, col = case.color)
    points(alter.xy, col = case.color)
    text(origin.xy, labels = x$origin, pos = 1, col = case.color)
    text(alter.xy, labels = end.case, pos = 1, col = case.color)
    title(main = paste("Case", x$origin, "to Case", end.case))
  } else if (x$type == "pumps") {
    case.color <- "blue"
    title(main = paste("Pump", x$origin, "to Pump", end.sel))
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

  arrows(ego.xy$x, ego.xy$y, alter.xy$x, alter.xy$y, length = 0.1,
    col = case.color, code = 3)

  distance <- stats::dist(dat)

  if (is.null(x$unit)) {
    title(sub = paste(round(distance, 2), "units"))
  } else if (x$unit == "meter") {
    title(sub = paste(round(cholera::unitMeter(distance, "meter"), 2),
      "meters"))
  } else if (x$unit == "yard") {
    title(sub = paste(round(cholera::unitMeter(distance, "yard"), 2), "yards"))
  }
}
