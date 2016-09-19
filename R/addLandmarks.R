#' Add landmarks to map.
#'
#' Adds Golden Square, Lion Brewery, St James Workhouse, St Luke's Church (Henry Whitehead), Soho Square, Falconberg Court & Mews, 18 Sackville (John Snow residence) and 28 Dean Street (Karl Marx residence) to an existing plot. Falconberg Court & Mews is technically an isolate in the network of roads. Consequently, both are technically unreachable.
#' @return Add base R points and text to a graphics plot.
#' @seealso \code{\link[cholera]{snowMap}}, \code{\link[cholera]{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addLandmarks()

addLandmarks <- function() {
 # 28 Dean Street
  marx <- data.frame(x = 17.3855, y = 13.371 )
  text(marx$x, marx$y, labels = "Karl\nMarx", cex = 0.5)
  points(marx$x, marx$y, pch = 15, cex = 1/3)

  # 18 Sackville Street
  snow <- data.frame(x = 10.22414, y = 4.383851)
  text(snow$x, snow$y, labels = "John\nSnow", cex = 0.5)
  points(snow$x, snow$y, pch = 15, cex = 1/3)

  # St. Luke's Church; Henry Whitehead Assistant Curate
  st.luke <- data.frame(x = 14.94156, y = 11.25313)
  text(st.luke$x, st.luke$y, labels = "St. Luke's\nChurch", cex = 0.5)
  points(st.luke$x, st.luke$y, pch = 15, cex = 1/3)

  # Soho Square
  soho.sq <- data.frame(x = 18.07044, y = 15.85703)
  text(soho.sq$x, soho.sq$y, labels = "Soho\nSquare", cex = 0.5)

  # Golden Square
  golden.sq <- data.frame(x = 11.90927, y = 8.239483)
  text(golden.sq$x, golden.sq$y, labels = "Golden\nSquare", cex = 0.5)

  # St. James Workhouse
  st.james <- data.frame(x = 11.5, y = 13.48414 )
  text(st.james$x, st.james$y, labels = "St James\nWorkhouse", cex = 0.5)

  # Lion Brewery (Huggins proprietors)
  brewery <- data.frame(x = 13.9022, y = 11.87315)
  text(brewery$x, brewery$y, labels = "Lion'\nBrewery", cex = 0.5)
  points(brewery$x, brewery$y, pch = 15, cex = 1/3)

  # Falconberg Court and Mews
  Falconberg <- data.frame(x = 19.5, y = 17.184)
  text(Falconberg$x, Falconberg$y, labels = "Falconberg\nCourt & Mews", cex = 0.5)
}

#' Add 2D kernel density contours.
#'
#' Uses KernSmooth::bkde2D().
#' @param bandwidth Numeric. bandwidth for kernel density esitmation.
#' @param color Character. Color of cotour lines.
#' @param line.type Character. Line type for contour.
#' @param data Character. NULL, uses \code{fatalities.unstacked}. "address" uses \code{fatalities.address}. "stacked" uses \code{fatalities}.
#' @return Add contours to a graphics plot.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addKernelDensity()

addKernelDensity <- function(bandwidth = 0.5, color = "black",
  line.type = "solid", data = NULL) {

  if (is.null(data) == FALSE) {
    if (all(data %in% c("address", "stacked") == FALSE)) {
      stop('If specified, "output" must either be "address" or "stacked".')
    }
  }

  bw.value <- bandwidth
  bw <- rep(bw.value, 2)

  if (is.null(data)) {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.unstacked[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "address") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities.address[, c("x", "y")],
      bandwidth = bw)
  } else if (data == "stacked") {
    kde2d <- KernSmooth::bkde2D(cholera::fatalities[, c("x", "y")],
      bandwidth = bw)
  }

  contour(x = kde2d$x1, y = kde2d$x2, z = kde2d$fhat, col = color,
    lty = line.type, add = TRUE)
}

#' Add plague pit (Marshall Street).
#'
#' Draws a polygon that approximates the plague pit located around Marshall Street. From Vestry Report map.
#' @param col Character. Color of circle.
#' @param lty Character. Type of line for circle.
#' @return Adds a polygon, based on multiple base R line segments, to a graphics plot.
#' @seealso \code{\link[cholera]{addLandmarks}}, \code{\link[cholera]{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addPlaguePit()
addPlaguePit <- function(col = "black", lty = "solid") {
  polygon(cholera::plague.pit, border = col, lty = lty)
}

#' Add Voronoi cells.
#'
#' Compute and draws Voronoi cells using deldir::deldir().

#' @param select Numeric. Default is NULL and all pumps are used. Ortherwise, selection by a vector of numeric IDs: 1 to 13 for \code{pumps}; 1 to 14 for \code{pumps.vestry}.
#' @param vestry Logical. FALSE for original 13 pumps. TRUE for 14 pumps in Vestry Report.
#' @param col Character. Color of borders.
#' @param lty Character. Type of line for borders.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addVoronoi()
addVoronoi <- function(select = NULL, vestry = FALSE, col = "black",
  lty = "solid") {

  if (is.null(select)) {
    if (!vestry) {
      dat <- cholera::pumps[, c("x", "y")]
    } else {
      dat <- cholera::pumps.vestry[, c("x", "y")]
    }
  } else {
    if (!vestry) {
      if (is.numeric(select) == FALSE | any(abs(select) %in% 1:13) == FALSE) {
        stop("For 'pumps', 'select' must be a vector with 1 >= |x| <= 13.")
      } else {
        dat <- cholera::pumps[select, c("x", "y")]
      }
    }
    if (vestry) {
      if (is.numeric(select) == FALSE | any(abs(select) %in% 1:14) == FALSE) {
        stop("For 'pumpsB', 'select' must be a vector with 1 >= |x| <= 14.")
      } else {
        dat <- cholera::pumps.vestry[select, c("x", "y")]
      }
    }
  }

  cells <- deldir::deldir(dat, rw = c(range(cholera::roads$x),
                                      range(cholera::roads$y)))
  plot(cells, add = TRUE, wline = "tess", wpoints = "none", col = col,
       lty = lty)
}

#' Add Whitehead's Broad Street pump neighborhood.
#'
#' Adds a circle with a given radius from a specified water pump. By default, the function draws Whitehead's Broad Street pump neighborhood: a circle, centered on the Broad Street pump, with a radius of 210 yards.
#' @param radius Numeric. Distance from a pump in yards
#' @param pump Character. The name of the pump, the street name where it is located. See \code{pumps} or \code{pumps.vestry}.
#' @param col Character. Color of circle.
#' @param lty Character. Circle line type.
#' @return Draws a circle, based on multiple line segments, to a graphics plot.
#' @seealso \code{\link[cholera]{addLandmarks}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addWhitehead()
addWhitehead <- function(radius = 210, pump = "Broad Street", col = "black",
  lty = "solid") {

  r <- radius / 59
  unit.base <- 100
  unit.radians <- 2 * pi / unit.base
  circumference.x <- cholera::pumps[cholera::pumps$street == pump, "x"] +
    r * cos(0:unit.base * unit.radians)
  circumference.y <- cholera::pumps[cholera::pumps$street == pump, "y"] +
    r * sin(0:unit.base * unit.radians)
  lines(circumference.x, circumference.y, col = col, lty = lty)
}
