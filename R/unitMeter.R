#' Convert nominal map distance to meters or yards.
#'
#' A best guess estimate.
#' @param x Numeric. Nominal map distance.
#' @param output.unit Character. Unit of distance: "meter", "yard" or "nominal". "nominal" returns the map's nominal scale. See \code{vignette("roads")} for information on conversion.
#' @param input.unit Character. "nominal" or "latlong".
#' @export

unitMeter <- function(x, output.unit = "meter", input.unit = "nominal") {
  if (all(input.unit %in% c("nominal", "latlong") == FALSE)) {
    stop('input.unit must be "nominal" or "latlong".')
  }

  if (is.numeric(x) == FALSE) {
    stop('x must be numeric.')
  }

  if (input.unit == "nominal") {
    sel <- cholera::road.segments$name == "Carnaby Street"
    carnaby <- cholera::road.segments[sel, ]
  } else if (input.unit == "latlong") {
    sel <- cholera::road.segments2$name == "Carnaby Street"
    carnaby <- cholera::road.segments2[sel, ]
  }

  carnaby <- carnaby[-c(8:9), ]

  carnaby.ft <- 463
  foot.unit <- carnaby.ft / stLength(carnaby, input.unit = input.unit)
  yard.unit <- foot.unit / 3
  meter.unit <- foot.unit / 3.281

  if (output.unit == "meter") {
    x * meter.unit
  } else if (output.unit == "yard") {
    x * yard.unit
  } else if (output.unit == "nominal") {
    x
  } else stop('output.unit must be "meter", "yard" or "nominal".')
}

#' Compute total length of roads.
#'
#' @param dat Object. data frame.
#' @param input.unit Character. "nominal" or "latlong".
#' @export

stLength <- function(dat, input.unit = "nominal") {
  d <- vapply(seq_len(nrow(dat)), function(i) {
    if (input.unit == "nominal") {
      p1 <- dat[i, c("x1", "y1")]
      p2 <- dat[i, c("x2", "y2")]
    } else if (input.unit == "latlong") {
      p1 <- dat[i, c("lon1", "lat1")]
      p2 <- dat[i, c("lon2", "lat2")]
    }
    vars <- c("x", "y")
    names(p1) <- vars
    names(p2) <- vars
    coords <- rbind(p1, p2)
    stats::dist(coords)
  }, numeric(1L))
  sum(d)
}
