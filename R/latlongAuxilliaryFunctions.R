#' Convert case IDs to numeric.
#'
#' @param x Object. table() object.
#' @export

caseNumber <- function(x) as.numeric(names(x))

#' Compute rectangle vertices.
#'
#' @param x Object. Points/pixel count.
#' @export

kmeansRectanlge <- function(x) {
  if (length(unique(x)) > 1) {
    km <- stats::kmeans(x, 2)
    km.df <- data.frame(ct = x, cluster = km$cluster)
    sel <- km.df[km.df$cluster == which.max(km$centers), ]
    as.numeric(row.names(sel))
  } else seq_along(x)
}

#' Extract points from GeoTiff.
#'
#' @param x Object. GeoTIFF.
#' @export

pointsFromGeoTIFF <- function(x) {
  ras <- raster::raster(x)
  pts <- raster::rasterToPoints(ras)
  data.frame(pts)
}

#' Index of subsets.
#'
#' @param max.ct Integer. Upper count of observations.
#' @param bin.size Integer. bin size size of subgroups.
#' @export

pointIndex <- function(max.ct = 321, bin.size = 50) {
  alpha <- seq(1, max.ct, bin.size)
  omega <- c(alpha[-1] - 1, max.ct)
  data.frame(start = alpha, stop = omega)
}

#' Estimate of georeferencing rotation (radians).
#'
#' QGIS georeferencing realigns map: left side approximately parallel to y-axis.
#' @param id1 Numeric. Road segment endpoint ID. Margaret Street.
#' @param id2 Numeric. Road segment endpoint ID. Phoenix Yard.
#' @note The two default points are the first two observations on the top left.
#' @export

referenceRadian <- function(id1 = 66, id2 = 171) {
  rd <- cholera::roads
  # rd[order(rd$x, rd$y), ] # first two observations on top left side
  x1 <- rd[rd$id == id1, "x"]
  y1 <- rd[rd$id == id1, "y"]
  x2 <- rd[rd$id == id2, "x"]
  y2 <- rd[rd$id == id2, "y"]
  atan((x1 - x2) / (y2 - y1))
}

#' Compute radians between observed point and centroid of 'roads'.
#'
#' @param points.data Object. Data frame of centroid and point.
#' @export

radians <- function(points.data) {
  ols <- stats::lm(y ~ x, data = points.data)
  segment.slope <- stats::coef(ols)[2]
  atan(segment.slope)
}

#' Rotate points (prototype).
#'
#' @param id Numeric. Road segment endpoint ID.
#' @param dataset Character. "roads", "fatalities", "fatalities.address", "pumps", or "pumps.vestry".
#' @param unique.coords Logical. Use unique coordinates.
#' @export

rotatePoint <- function(id = 1, dataset = "roads", unique.coords = TRUE) {
  rd <- cholera::roads[cholera::roads$name != "Map Frame", ]
  rd <- rd[order(rd$x, rd$y), ]

  if (unique.coords) {
    rd$point.id <- paste0(rd$x, "-", rd$y)
    rd <- rd[!duplicated(rd$point.id), ]
  }

  center <- data.frame(x = mean(range(rd$x)), y = mean(range(rd$y)))

  if (dataset == "roads") {
    points.data <- rbind(center, rd[rd$id == id, c("x", "y")])
  } else if (dataset == "fatalities") {
    sel <- cholera::fatalities$case == id
    points.data <- rbind(center, cholera::fatalities[sel, c("x", "y")])
  } else if (dataset == "fatalities.address") {
    sel <- cholera::fatalities.address$anchor == id
    points.data <- rbind(center, cholera::fatalities.address[sel, c("x", "y")])
  } else if (dataset == "pumps") {
    sel <- cholera::pumps$id == id
    points.data <- rbind(center, cholera::pumps[sel, c("x", "y")])
  } else if (dataset == "pumps.vestry") {
    sel <- cholera::pumps.vestry$id == id
    points.data <- rbind(center, cholera::pumps.vestry[sel, c("x", "y")])
  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address",'
    msg2 <- '"pumps", or "pumps.vestry".'
    stop(paste(msg1, msg2))
  }

  theta <- radians(points.data)
  h <- stats::dist(points.data)
  theta.delta <- referenceRadian()

  if (points.data$x[1] - points.data$x[2] >= 0) {
    x.prime <- c(center$x - cos(theta - theta.delta) * h)
    y.prime <- c(center$y - sin(theta - theta.delta) * h)
  } else {
    x.prime <- c(center$x + cos(theta - theta.delta) * h)
    y.prime <- c(center$y + sin(theta - theta.delta) * h)
  }

  data.frame(x = x.prime, y = y.prime, row.names = NULL)
}

#' Create subsetted PDFs (prototype).
#'
#' Reduce over-printing of points.
#' @param path Character. e.g., "~/Documents/Data/"
#' @param dataset Object. A 'cholera' dataset e.g., roads.
#' @export

subsetPDF <- function(path, dataset = "fatalities.address") {
  framework <- cholera::roads[cholera::roads$name != "Map Frame", ]

  if (dataset == "roads") {
    dat <- framework
    dat$point.id <- paste0(dat$x, "-", dat$y)
    dat <- dat[!duplicated(dat$point.id), ]
    file.nm <- "road"
  } else if (dataset == "fatalities") {
    dat <- cholera::fatalities
    file.nm <- "fatality"
  } else if (dataset == "fatalities.address") {
    dat <- cholera::fatalities.address
    file.nm <- "address"
  } else if (dataset == "pumps") {
    dat <- cholera::pumps
    file.nm <- "pump"
  } else if (dataset == "pumps.vestry") {
    dat <- cholera::pumps.vestry
    file.nm <- "pump.vestry"
  } else {
    msg1 <- 'dataset must be "roads", "fatalities", "fatalities.address", '
    msg2 <- '"pumps" or "pumps.vestry".'
    stop(msg1, msg2, call. = FALSE)
  }

  if (dataset %in% c("roads", "fatalities", "fatalities.address")) {
    idx <- pointIndex(nrow(dat))
    num.id <- seq_len(nrow(idx))

    if (any(num.id >= 10)) {
      num.id <- c(paste0("0", num.id[num.id < 10]), num.id[num.id >= 10])
    } else {
      num.id <- paste0("0", num.id)
    }

    invisible(lapply(seq_along(num.id), function(i) {
      pre <- paste0(file.nm, ".")
      post <- ".pdf"
      grDevices::pdf(file = paste0(path, pre, num.id[i], post))
      plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n",
        xlab = NA, ylab = NA, bty = "n")
      sel <- idx[i, "start"]:idx[i, "stop"]
      points(dat[sel, c("x", "y")], pch = 15, cex = 0.2)
      grDevices::dev.off()
    }))
  } else {
    pre <- file.nm
    post <- ".01.pdf"
    grDevices::pdf(file = paste0(path, pre, post))
    plot(framework$x, framework$y, pch = NA, xaxt = "n", yaxt = "n", xlab = NA,
      ylab = NA, bty = "n")
    points(dat[, c("x", "y")], pch = 15, cex = 0.2)
    grDevices::dev.off()
  }
}
