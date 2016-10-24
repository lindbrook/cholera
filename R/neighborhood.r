#' Observed walking path neighborhood data.
#'
#' Neighborhoods are based on the shortest paths between a fatality's address and its nearest pump.
#' @param selection Numeric. Default is NULL; all pumps are used. Otherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}
#' @param vestry Logical. TRUE uses the 14 pumps from the Vestry Report. FALSE uses the 13 in the original map.
#' @export

neighborhood <- function(selection = NULL, vestry = FALSE) {
  if (vestry) {
    if (is.null(selection) == FALSE) {
      if (any(abs(selection) %in% 1:14 == FALSE)) {
        stop('With "vestry = TRUE", "selection" must be between 1 and 14.')
      }
    }
  } else {
    if (is.null(selection) == FALSE ) {
      if (any(abs(selection) %in% 1:13 == FALSE)) {
        stop('With "vestry = FALSE", "selection" must be between 1 and 13.')
      }
    }
  }

  # roads <- cholera::roads[cholera::roads$street %in% cholera::border == FALSE, ]
  # map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
  # roads.list <- split(roads[, c("x", "y")], roads$street)
  # border.list <- split(map.frame[, c("x", "y")], map.frame$street)

  # integrate pumps into road network
  if (vestry) {
    # colors <- snowColors(vestry = TRUE)
    pump.road.segments <- pumpIntegrator(cholera::ortho.proj.pump.vestry)
    # pump.coordinates <- pumpCoordinates(vestry = TRUE)

  } else {
    # colors <- snowColors()
    pump.road.segments <- pumpIntegrator()
    # pump.coordinates <- pumpCoordinates()
  }
  pump.road.segments
}

roadSegments <- function() {
  roadsB <- cholera::roads[cholera::roads$street %in%
    cholera::border == FALSE, ]
  out <- lapply(unique(roadsB$street), function(i) {
    dat <- roadsB[roadsB$street == i, ]
    names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
    seg.data <- dat[-1, c("x1", "y1")]
    names(seg.data) <- c("x2", "y2")
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  do.call(rbind, out)
}

pumpIntegrator <- function(pump.data = cholera::ortho.proj.pump,
  road.segments = roadSegments()) {

  pump.segments <- pump.data$road.segment
  mat <- matrix(0, ncol = ncol(road.segments), nrow = 2 * length(pump.segments))
  road.pump.data <- data.frame(mat)
  start.pt <- seq(1, nrow(road.pump.data), 2)
  end.pt <- seq(2, nrow(road.pump.data), 2)

  for (i in seq_along(pump.segments)) {
    road.data <- road.segments[road.segments$id == pump.segments[i], ]
    pump.coords <- pump.data[pump.data$road.segment == pump.segments[i],
      c("x.proj", "y.proj")]

    temp <- road.data[, names(road.data) %in% c("x1", "y1") == FALSE]
    temp <- cbind(temp[, c("street", "n")], pump.coords,
      temp[, c("id", "name", "x2", "y2")])

    names(temp)[names(temp) %in% c("x.proj", "y.proj")] <- c("x1", "y1")
    road.data[, c("x2", "y2")] <- pump.coords
    temp <- rbind(road.data, temp)
    temp$id <- paste0(road.data$id, letters[seq_len(nrow(temp))])
    road.pump.data[start.pt[i]:end.pt[i], ] <- temp
  }

  names(road.pump.data) <- names(road.segments)
  road.segments <- road.segments[road.segments$id %in% pump.segments == FALSE, ]
  out <- rbind(road.segments, road.pump.data)
  out <- out[order(out$street, out$id), ]
  out$node1 <- paste0(out$x1, "-", out$y1)
  out$node2 <- paste0(out$x2, "-", out$y2)
  out$d <- sqrt((out$x1 - out$x2)^2 + (out$y1 - out$y2)^2)
  out
}
