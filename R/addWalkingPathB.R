#' Add walking path from case/landmark to nearest or selected pump. (prototype)
#'
#' @param origin Numeric. Vector of origin(s) (numeric or case/landmark name).
#' @param destination Numeric. Vector of destination(s) (numeric or landmark/pump name).
#' @param type Character. Path case to pump. FALSE is all other combinations of cases, landmarks and pumps.
#' @param vestry Logical. \code{TRUE} uses the 14 pumps from the map in the Vestry Report. \code{FALSE} uses the 13 pumps from the original map.
#' @param latlong Logical.
#' @param weighted Logical. \code{TRUE} computes shortest path in terms of road length. \code{FALSE} computes shortest path in terms of the number of nodes.
#' @param distance.unit Character. Unit of distance: "meter" or "yard".
#' @param time.unit Character. "hour", "minute", or "second".
#' @param walking.speed Numeric. Walking speed in km/hr.
#' @param include.landmarks Logical. Include landmarks as cases.

#' @param mileposts Logical. Plot mile/time posts.
#' @param milepost.unit Character. "distance" or "time".
#' @param milepost.interval Numeric. Mile post interval unit of distance (yard or meter) or unit of time (seconds).
#' @param alpha.level Numeric. Alpha level transparency for path: a value in [0, 1].
#' @param ... Additional plotting parameters.
#' @importFrom geosphere distGeo
#' @export

addWalkingPathB <- function(origin = 1, destination = NULL, type = "case-pump",
  vestry = FALSE, latlong = FALSE, weighted = TRUE, distance.unit = "meter",
  time.unit = "second", walking.speed = 5, include.landmarks = TRUE,
  mileposts = TRUE, milepost.unit = "distance", milepost.interval = NULL,
  alpha.level = 1, ...) {

  args <- list(origin = origin,
               destination = destination,
               type = type,
               vestry = vestry,
               latlong = latlong,
               weighted = weighted,
               distance.unit = distance.unit,
               time.unit = time.unit,
               walking.speed = walking.speed,
               include.landmarks = include.landmarks)

  x <- do.call("walkingPathB", args)

  path.data <- x$data
  type <- x$data$type
  orig <- path.data$orig
  dest <- path.data$dest
  destination <- x$destination
  colors <- snowColors(x$vestry)
  dat <- x$path
  ds <- x$ds
  distance.unit <- x$distance.unit
  latlong <- x$latlong
  time.unit <- x$time.unit
  walking.speed <- x$walking.speed
  pmp <- x$pmp
  edges <- x$edges

  if (distance.unit == "meter") {
    d.unit <- "m"
  } else if (distance.unit == "yard") {
    d.unit <- "yd"
  }

  if (milepost.unit == "distance") {
    path.length <- sum(ds)
  } else if (milepost.unit == "time") {
    path.length <- (3600L * sum(ds)) / (1000L * walking.speed)
  }

  fatality <- cholera::fatalities
  fatality.ortho <- cholera::latlong.ortho.addr
  land <- cholera::landmarksB

  if (latlong) {
    ew <- "lon"
    ns <- "lat"
  } else {
    ew <- "x"
    ns <- "y"
  }

  vars <- c(ew, ns)

  if (type == "case-pump") {
    p.sel <- paste0("p", path.data$dest)
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
        sel <- cholera::landmark.squaresB$name == obs.sq
        text(cholera::landmark.squaresB[sel, c(ew, ns)], labels = sq.label,
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
          sel <- cholera::landmark.squaresB$name == path.data$dest.nm
          label.dat <- cholera::landmark.squaresB[sel, ]
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

  points(dat[1, vars], pch = 0)
  points(dat[nrow(dat), vars], pch = 0)

  drawPathB(dat, case.color, latlong)

  d <- paste(round(path.length, 1), d.unit)
  t <- paste(round(x$data$time, 1), paste0(time.unit, "s"), "@", walking.speed,
             "km/hr")

  if (is.null(milepost.interval)) {
    if (milepost.unit == "distance") {
      milepost.interval <- 50
    } else if (milepost.unit == "time") {
      milepost.interval <- 60
    }
  }

  milepost.data <- milePostsB(path.data, dat, destination, distance.unit, ds,
    latlong, milepost.unit, milepost.interval, time.unit, walking.speed)

  seg.data <- milepost.data$seg.data

  # last/final arrow ("last mile")
  arrows(seg.data[1, paste0(ew, 2)], seg.data[1, paste0(ns, 2)],
         seg.data[1, paste0(ew, 1)], seg.data[1, paste0(ns, 1)],
         length = 0.0875, lwd = 3, col = case.color)

  if (milepost.unit == "distance") {
    if (distance.unit == "meter") {
      post.info <- paste("posts at", milepost.interval, "m intervals")
    } else if (distance.unit == "yard") {
      post.info <- paste("posts at", milepost.interval, "yd intervals")
    }
  } else if (milepost.unit == "time") {
    post.info <- paste("posts at", milepost.interval, "sec intervals")
  } else {
    stop('"milepost.unit" muster either be "distance" or "time".')
  }

  if (mileposts) {
    if (path.length > milepost.interval) {
      arrow.head <- milepost.data$arrow.head
      arrow.tail <- milepost.data$arrow.tail
    }

    if (path.length >= milepost.interval) {
      cutpoint <- ifelse(latlong, -13L, -6L)
      zero.length.ew <- log(abs(arrow.tail[, ew] - arrow.head[, ew])) < cutpoint
      zero.length.ns <- log(abs(arrow.tail[, ns] - arrow.head[, ns])) < cutpoint

      if (any(zero.length.ew | zero.length.ns)) {
        zero.id <- unique(row.names(arrow.head[zero.length.ew, ]),
                          row.names(arrow.head[zero.length.ns, ]))

        angle <- vapply(zero.id, function(id) {
          zero.arrow <- rbind(arrow.tail[id, vars], arrow.head[id, vars])
          if (latlong) ols <- stats::lm(lat ~ lon, data = zero.arrow)
          else ols <- stats::lm(y ~ x, data = zero.arrow)
          slope <- stats::coef(ols)[2]
          theta <- atan(slope)
          theta * 180L / pi
        }, numeric(1L))

        invisible(lapply(seq_along(zero.id), function(i) {
          text(arrow.head[zero.id[i], vars], labels = "<", srt = angle[i],
               col = case.color, cex = 1.25)
        }))

        arrow.head <- arrow.head[!row.names(arrow.head) %in% zero.id, ]
        arrow.tail <- arrow.tail[!row.names(arrow.tail) %in% zero.id, ]
      }

      arrows(arrow.tail[, ew], arrow.tail[, ns],
             arrow.head[, ew], arrow.head[, ns],
             length = 0.0875, lwd = 3, col = case.color)
    }

  }
}

