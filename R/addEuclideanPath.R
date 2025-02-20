#' Add Euclidean path from case/landmark to nearest or selected pump. (prototype)
#'
#' @param origin Numeric. Vector of origin(s) (numeric ID or character name landmark/pump ).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param case.set Character. "observed" or "expected".
#' @param location Character. For cases and pumps. "nominal", "anchor" or "orthogonal".
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.
#' @param long.title Logical. Tile with names.
#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @export

addEuclideanPath <- function(origin = 1, destination = NULL,
  type = "case-pump", vestry = FALSE, latlong = FALSE, case.set = "observed",
  location = "nominal", weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, include.landmarks = TRUE,
  long.title = FALSE, mileposts = TRUE, milepost.unit = "distance",
  milepost.interval = NULL, alpha.level = 1) {

  args <- list(origin = origin,
               destination = destination,
               type = type,
               vestry = vestry,
               latlong = latlong,
               case.set = case.set,
               location = location,
               weighted = weighted,
               distance.unit = distance.unit,
               time.unit = time.unit,
               walking.speed = walking.speed,
               include.landmarks = include.landmarks)

  x <- do.call("euclideanPath", args)

  path.data <- x$data
  type <- x$data$type
  ego.xy <- x$ego
  alter.xy <- x$alter
  dat <- rbind(alter.xy, ego.xy)
  pmp <- x$pmp
  orig <- path.data$origin
  dest <- path.data$destination

  colors <- snowColors(x$vestry)
  distance.unit <- x$distance.unit
  latlong <- x$latlong
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed

  if (distance.unit == "meter") {
    d.unit <- "m"
  } else if (distance.unit == "yard") {
    d.unit <- "yd"
  }

  if (milepost.unit == "distance") {
    path.length <- path.data$distance
  } else if (milepost.unit == "time") {
    path.length <- (3600L * path.data$distance) / (1000L * walking.speed)
  }

  fatality <- cholera::fatalities

  sqB <- cholera::landmark.squares
  sqB$road.segment <- NA
  sqB$x.lab <- sqB$x
  sqB$y.lab <- sqB$y
  sqB$lon.lab <- sqB$lon
  sqB$lat.lab <- sqB$lat
  sqB <- sqB[, names(cholera::landmarks)]
  land <- rbind(sqB, cholera::landmarks)

  if (latlong) {
    ew <- "lon"
    ns <- "lat"
  } else {
    ew <- "x"
    ns <- "y"
  }

  vars <- c(ew, ns)

  if (type == "case-pump") {
    p.sel <- paste0("p", path.data$destination)
    case.color <- grDevices::adjustcolor(colors[p.sel], alpha.f = alpha.level)
  } else {
    case.color <- "blue"
  }

  if (type %in% c("case-pump", "cases")) {
    if (orig < 1000L) {
      points(fatality[fatality$case == orig, vars], col = "red")
      text(fatality[fatality$case == orig, vars], pos = 1, labels = orig,
        col = "red")
    } else if (orig >= 1000L) {
      points(land[land$case == orig, vars], col = "red")
      land.tmp <- land[land$case == orig, ]

      if (grepl("Square", land.tmp$name)) {
        sq.label <- unlist(strsplit(land.tmp$name, "-"))[1]
        label.parse <- unlist(strsplit(sq.label, "[ ]"))
        sq.label <- paste0(label.parse[1], "\n", label.parse[2])
        obs.sq <- paste(label.parse, collapse = " ")
        sel <- cholera::landmark.squares$name == obs.sq
        text(cholera::landmark.squares[sel, c(ew, ns)], labels = sq.label,
          col = "red", cex = 0.8)
      } else {
        label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
        names(label.dat) <- vars
        if (grepl("St", land.tmp$name)) {
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          land.label <- paste0(paste(label.parse[1], label.parse[2]), "\n",
            label.parse[3])
        } else {
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          if (length(label.parse) == 2) {
            land.label <- paste0(label.parse[1], "\n", label.parse[2])
          } else if (length(label.parse) == 3) {
            land.label <- paste0(label.parse[1], "\n", label.parse[2], "\n",
                                 label.parse[3])
          }
        }
        text(label.dat, labels = land.label, col = "red", cex = 0.8)
      }
    }

    if (type == "cases") {
      if (dest < 1000L) {
        points(fatality[fatality$case == dest, vars], col = "red")
        text(fatality[fatality$case == dest, vars], pos = 1, labels = dest,
          col = "red")
      } else if (dest >= 1000L) {
        points(land[land$case == dest, vars], col = "red")
        land.tmp <- land[land$case == dest, ]
        if (grepl("Square", land.tmp$name)) {
          sel <- cholera::landmark.squares$name == path.data$destination.nm
          label.dat <- cholera::landmark.squares[sel, ]
          label.parse <- unlist(strsplit(label.dat$name, "[ ]"))
          sq.label <- paste0(label.parse[1], "\n", label.parse[2])
          text(label.dat[, c(ew, ns)], labels = sq.label, col = "red",
            cex = 0.8)
        } else if (land.tmp[, ew] != land.tmp[, paste0(ew, ".lab")]) {
          label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
          names(label.dat) <- vars
          if (grepl("St", land.tmp$name)) {
            label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
            land.label <- paste0(paste(label.parse[1], label.parse[2]), "\n",
                                       label.parse[3])
          } else {
            label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
            if (length(label.parse) == 2) {
              land.label <- paste0(label.parse[1], "\n", label.parse[2])
            } else if (length(label.parse) == 3) {
              land.label <- paste0(label.parse[1], "\n", label.parse[2], "\n",
                                   label.parse[3])
            }
          }
          text(label.dat, labels = land.label, col = "red", cex = 0.8)
        } else {
          label.dat <- land.tmp[, c(paste0(ew, ".lab"), paste0(ns, ".lab"))]
          names(label.dat) <- vars
          label.parse <- unlist(strsplit(land.tmp$name, "[ ]"))
          land.label <- paste0(label.parse[1], "\n", label.parse[2])
          text(land[land$case == dest, vars], labels = land.label, col = "red",
            cex = 0.8)
        }
      }
    }
  }

  if (mileposts) {
    if (is.null(milepost.interval)) {
      if (milepost.unit == "distance") {
        milepost.interval <- 50
      } else if (milepost.unit == "time") {
        milepost.interval <- 60
      }
    }

    if (milepost.unit == "distance") {
      h <- seq(0, path.data$distance, milepost.interval)
      if (isFALSE(latlong)) h <- h / unitMeter(1)
    } else if (milepost.unit == "time") {
      h <- seq(0, path.data$time, milepost.interval)
      if (isFALSE(latlong)) {
        h <- h * 1000 * x$walking.speed / 60^2 / unitMeter(1)
      }
    } else {
      stop('Specify milepost.unit', call. = FALSE)
    }

    if (latlong) ols <- stats::lm(lat ~ lon, data = dat)
    else ols <- stats::lm(y ~ x, data = dat)
    edge.slope <- stats::coef(ols)[2]
    theta <- ifelse(is.na(edge.slope), pi / 2, atan(edge.slope))

    if (latlong) {
      post.coords <- latlongEuclideanPosts(ego.xy, alter.xy, h, ew, ns)
    } else {
      post.coords <- quandrantCoordinates(dat[2:1, ], h, theta)
    }

    arrow.data <- data.frame(x = c(post.coords[, ew], ego.xy[, ew]),
                             y = c(post.coords[, ns], ego.xy[, ns]))

    arrow.list <- lapply(seq_len(nrow(arrow.data) - 1), function(i) {
      a.data <- cbind(arrow.data[i, ], arrow.data[i + 1, ])
      stats::setNames(a.data, c(paste0(c(ew, ns), 1), paste0(c(ew, ns), 2)))
    })

    invisible(lapply(arrow.list, function(seg) {
      arrows(seg[, paste0(ew, 1)], seg[, paste0(ns, 1)],
             seg[, paste0(ew, 2)], seg[, paste0(ns, 2)],
             length = 0.075, col = case.color, lwd = 3, code = 1)
    }))
  } else {
    arrows(ego.xy[, ew], ego.xy[, ns], alter.xy[, ew], alter.xy[, ns],
      col = case.color, lwd = 3, length = 0.075)
  }
}
