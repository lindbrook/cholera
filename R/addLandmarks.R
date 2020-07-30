#' Add landmarks to plot.
#'
#' @param text.size Numeric. cex for text labels.
#' @param highlight.perimeter Logical. Highlight Lion Brewery and Model Housing.
#' @note The location of 18 Sackville Street and 28 Dean Street are approximate. Falconberg Court & Mews form an isolate: they are not part of the network of roads and are technically unreachable. Adam and Eve Court and its pump also form an isolate.
#' @return Base R points and text.
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addLandmarks()

addLandmarks <- function(text.size = 0.5, highlight.perimeter = TRUE) {
  # 28 Dean Street
  marx <- data.frame(x = 17.3855, y = 13.371)
  text(marx$x, marx$y, labels = "Karl\nMarx", cex = text.size)
  points(marx$x, marx$y, pch = 15, cex = 1/3)

  # 18 Sackville Street
  snow <- data.frame(x = 10.22414, y = 4.383851)
  text(snow$x, snow$y, labels = "John\nSnow", cex = text.size)
  points(snow$x, snow$y, pch = 15, cex = 1/3)

  # St. Luke's Church; Henry Whitehead Assistant Curate
  st.luke <- data.frame(x = 14.94156, y = 11.25313)
  text(st.luke$x, st.luke$y, labels = "St. Luke's\nChurch", cex = text.size,
    pos = 4, offset = 0.25)
  points(st.luke$x, st.luke$y, pch = 15, cex = 1/3)

  # Soho Square
  soho.sq <- data.frame(x = 18.07044, y = 15.85703)
  text(soho.sq$x, soho.sq$y, labels = "Soho\nSquare", cex = text.size)

  # Golden Square
  golden.sq <- data.frame(x = 11.90927, y = 8.239483)
  text(golden.sq$x, golden.sq$y, labels = "Golden\nSquare", cex = text.size)

  # St. James Workhouse
  right <- cholera::road.segments[cholera::road.segments$name ==
    "St James Workhouse", c("x1", "y1")]
  left <- cholera::road.segments[cholera::road.segments$id == "201-1",
    c("x2", "y2")]
  dat <- stats::setNames(data.frame(rbind(unlist(right), unlist(left))),
    c("x", "y"))
  h <- c(stats::dist(dat))
  ols <- stats::lm(y ~ x, dat)
  segment.slope <- stats::coef(ols)[2]
  theta <- atan(segment.slope)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)
  x.new <- left$x2 + delta.x
  y.new <- left$y2 + delta.y
  # st.james <- data.frame(x = 11.5, y = 13.48414 )
  text(x.new, y.new, labels = "St James\nWorkhouse", cex = text.size)

  # Lion Brewery (Huggins' proprietors)
  brewery <- data.frame(x = 13.9022, y = 11.87315)
  text(brewery$x, brewery$y, labels = "Lion\nBrewery", cex = text.size)
  points(brewery$x, brewery$y, pch = 15, cex = 1/3)

  # Falconberg Court and Mews (isolate)
  Falconberg <- data.frame(x = 19.5, y = 17.184)
  text(Falconberg$x, Falconberg$y, labels = "Falconberg\nCourt & Mews",
    cex = text.size)

  # Adam and Eve Court (isolate)
  adam.eve <- cholera::roads[cholera::roads$name == "Adam and Eve Court",
    c("x", "y")]
  text(stats::quantile(adam.eve[, "x"], 0.25),
       stats::quantile(adam.eve[, "y"], 0.25),
       labels = "Adam & Eve\nCourt", cex = text.size)

  ## Edmund Cooper's Map ##

  nm <- c("x", "y")

  # Argyll House: Earl of Aberdeen
  # https://www.british-history.ac.uk/survey-london/vols31-2/pt2/pp250-267
  # https://www.british-history.ac.uk/old-new-london/vol4/pp235-246
  NW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "116-2", c("x2", "y2")], nm)
  NE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "144-1", c("x2", "y2")], nm)
  SW <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "161-1", c("x2", "y2")], nm)
  SE <- stats::setNames(cholera::road.segments[cholera::road.segments$id ==
    "161-1", c("x1", "y1")], nm)

  aberdeen <- segmentIntersection(NW$x, NW$y, SE$x, SE$y,
                                  NE$x, NE$y, SW$x, SW$y)

  points(aberdeen$x, aberdeen$y, pch = 15, cex = 1/3)
  text(aberdeen$x, aberdeen$y, labels = "Earl of\nAberdeen", cex = text.size)

  # 1) Marlborough Mews: Police Station

  # 2) Regent (opposite) at intersection with Little Argyll Street: Chapel

  # 4) Oxford Street (opposite) at intersection with Winsley Street:
  # Pantheon Bazaar`

  text(cholera::road.segments[cholera::road.segments$name == "Winsley Street",
    c("x2", "y2")], pos = 1, labels = "The\nPantheon", cex = text.size)

  # 5) 7 Cambridge Street at corner intersection with Broad Street: PH

  # 6) Adjacent south of Lion Brewery: Model Lodging Housing
  # (in process of erection)
  # http://www.workhouses.org.uk/model/

  sel <- cholera::road.segments$name == "Cock Court"
  rd.data <- cholera::road.segments[sel, c("x2", "y2")]
  NW <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$name == "Cock Court"
  rd.data <- cholera::road.segments[sel, c("x1", "y1")]
  NE <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$id == "259-1"
  rd.data <- cholera::road.segments[sel, c("x2", "y2")]
  SW <- stats::setNames(rd.data, nm)

  sel <- cholera::road.segments$id == "259-1"
  rd.data <- cholera::road.segments[sel, c("x1", "y1")]
  SE <- stats::setNames(rd.data, nm)

  model.lodging <- segmentIntersection(NW$x, NW$y, SE$x, SE$y,
                                       NE$x, NE$y, SW$x, SW$y)

  points(model.lodging$x, model.lodging$y, pch = 15, cex = 1/3)
  text(model.lodging$x, model.lodging$y, labels = "Model\nLodging",
    cex = text.size)

  # 7) Marshall Street Public Baths built 1851-2  (Marshall Street)
  # http://www.british-history.ac.uk/survey-london/vols31-2/pt2/pp196-208

  intersectionPoint <- function(seg1, seg2, sel = 1) {
    s1 <- cholera::road.segments[cholera::road.segments$id == seg1, ]
    s2 <- cholera::road.segments[cholera::road.segments$id == seg2, ]
    dat <- lapply(list(s1, s2), toDataFrame)
    ols <- lapply(dat, stats::lm, formula = y ~ x)
    coefs <- lapply(ols, stats::coef)
    x <- (coefs[[1]][1] - coefs[[2]][1]) / (coefs[[2]][2] - coefs[[1]][2])
    y <- coefs[[1]][1] + coefs[[1]][2] * x
    h <- c(stats::dist(rbind(s2[, c(paste0("x", sel), paste0("y", sel))],
                             c(x, y))))
    segment.slope <- stats::coef(ols[[2]])[2]
    theta <- atan(segment.slope)
    delta.x <- (h / 2) * cos(theta)
    delta.y <- (h / 2) * sin(theta)
    x.new <- x + delta.x
    y.new <- y + delta.y
    data.frame(x = x.new, y = y.new)
  }

  toDataFrame <- function(dat) {
    out <- data.frame(rbind(c(dat$x1, dat$y1), c(dat$x2, dat$y2)))
    stats::setNames(out, c("x", "y"))
  }

  public.baths <- intersectionPoint("201-2", "217-2", 1)
  text(public.baths, labels = "Public\nBaths", cex = text.size)

  # 8) Craven Chapel (Wesleyan)

  ep1 <- cholera::road.segments[cholera::road.segments$name == "Lowndes Court",
    c("x2", "y2")]
  ep2 <- cholera::road.segments[cholera::road.segments$id == "201-1",
    c("x2", "y2")]
  dat <- stats::setNames(rbind(ep1, ep2), nm)
  h <- c(stats::dist(dat))
  ols <- stats::lm(y ~ x, dat)
  segment.slope <- stats::coef(ols)[2]
  theta <- atan(segment.slope)
  delta.x <- (h / 2) * cos(theta)
  delta.y <- (h / 2) * sin(theta)
  x.new <- dat[1, "x"] + delta.x
  y.new <- dat[1, "y"] + delta.y
  text(x.new, y.new, labels = "Craven\nChapel", cex = text.size)

  if (highlight.perimeter) {
    lion.brewery.north <- "187-1"
    lion.brewery.south <- "225-1"
    lion.brewery.east <- c("197-1", "215-1")
    lion.brewery.west <- c("224-1", "226-1")

    model.housing.north <- lion.brewery.south
    model.housing.south <- "259-1"
    model.housing.east <- c("245-1", "245-2")
    model.housing.west <- "259-2"

    brewery <- c(lion.brewery.north, lion.brewery.south, lion.brewery.east,
      lion.brewery.west)
    model <- c(model.housing.north, model.housing.south, model.housing.east,
      model.housing.west)
    vars <- c("x1", "y1", "x2", "y2")

    landmarkPerimeter <- function(seg.id, col = "dodgerblue", lwd = 2) {
      lapply(seg.id, function(seg) {
        dat <- cholera::road.segments[cholera::road.segments$id == seg, vars]
        segments(dat$x1, dat$y1, dat$x2, dat$y2, col = col, lwd = lwd)
      })
    }

    invisible(lapply(c(brewery, model), landmarkPerimeter))
  }
}
