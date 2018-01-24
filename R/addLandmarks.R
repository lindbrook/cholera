#' Add landmarks.
#'
#' Adam and Eve Court (isolate), Golden Square, Lion Brewery, St James Workhouse, St Luke's Church (Henry Whitehead), Soho Square, Falconberg Court & Mews (isolate), 18 Sackville Street (John Snow residence) and 28 Dean Street (Karl Marx residence).
#' @note 18 Sackville Street and 28 Dean Street are approximate. Falconberg Court & Mews form an isolate: they are not part of the network of roads and are technically unreachable. Adam and Eve Court and its pump also form an isolate.
#' @return Add base R points and text to a graphics plot.
#' @param text.size Numeric. cex for text labels.
#' @seealso \code{\link{snowMap}},
#' \code{\link{addIndexCase}},
#' \code{\link{addKernelDensity}},
#' \code{\link{addPlaguePit}},
#' \code{\link{addSnow}},
#' \code{\link{addVoronoi}},
#' \code{\link{addWhitehead}}
#' @import graphics
#' @export
#' @examples
#' snowMap(add.landmarks = FALSE)
#' addLandmarks()

addLandmarks <- function(text.size = 0.5) {
  # 28 Dean Street
  marx <- data.frame(x = 17.3855, y = 13.371 )
  text(marx$x, marx$y, labels = "Karl\nMarx", cex = text.size)
  points(marx$x, marx$y, pch = 15, cex = 1/3)

  # 18 Sackville Street
  snow <- data.frame(x = 10.22414, y = 4.383851)
  text(snow$x, snow$y, labels = "John\nSnow", cex = text.size)
  points(snow$x, snow$y, pch = 15, cex = 1/3)

  # St. Luke's Church; Henry Whitehead Assistant Curate
  st.luke <- data.frame(x = 14.94156, y = 11.25313)
  text(st.luke$x, st.luke$y, labels = "St. Luke's\nChurch", cex = text.size)
  points(st.luke$x, st.luke$y, pch = 15, cex = 1/3)

  # Soho Square
  soho.sq <- data.frame(x = 18.07044, y = 15.85703)
  text(soho.sq$x, soho.sq$y, labels = "Soho\nSquare", cex = text.size)

  # Golden Square
  golden.sq <- data.frame(x = 11.90927, y = 8.239483)
  text(golden.sq$x, golden.sq$y, labels = "Golden\nSquare", cex = text.size)

  # St. James Workhouse
  st.james <- data.frame(x = 11.5, y = 13.48414 )
  text(st.james$x, st.james$y, labels = "St James\nWorkhouse", cex = text.size)

  # Lion Brewery (Huggins proprietors)
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
}
