#' Locate road by numerical ID.
#'
#' Highlight a road and its cases. See cholera::roads for numerical IDs and \code{vignette}("road.names") for details.
#' @param road.number Numeric or integer. A whole number between 1 and 528.
#' @param zoom Logical.
#' @param radius Numeric. Controls the degree of zoom.
#' @param cases Character. Plot cases: NULL, "anchors" or "all".
#' @param unit Character. Unit of measurement: "meter" or "yard". Default is NULL, which returns the map's native scale.
#' @return A base R graphics plot.
#' @seealso \code{\link{roads}}, \code{\link{road.segments}}, \code{\link{streetNameLocator}}, \code{vignette("road.names")}
#' @import graphics
#' @export
#' @examples
#' streetNumberLocator(243)
#' streetNumberLocator(243, zoom = TRUE)
#' streetNumberLocator(243, zoom = TRUE, radius = 0)

streetNumberLocator <- function(road.number, zoom = FALSE, radius = 1,
  cases = "anchors", unit = NULL) {

  if (is.numeric(road.number) == FALSE) {
    stop("road.number must be numeric.")
  }

  if (road.number %in% unique(cholera::roads$street) == FALSE) {
    stop("road.number must lie between 1 and 528.")
  }

  if (is.null(cases) == FALSE) {
    if (cases %in% c("anchors", "all") == FALSE)
      stop('If specified, "cases" must either be "anchors" or "all".')
  }

  roads.list <- split(cholera::roads[, c("x", "y")], cholera::roads$street)
  rng <- lapply(cholera::roads[cholera::roads$street == road.number,
       c("x", "y")], range)
  x.rng <- c(min(rng$x) - radius, max(rng$x) + radius)
  y.rng <- c(min(rng$y) - radius, max(rng$y) + radius)

  if (zoom == FALSE) {
    plot(cholera::fatalities[, c("x", "y")], xlim = range(cholera::roads$x),
      ylim = range(cholera::roads$y), pch = 15, cex = 0.5, col = "gray",
      asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))
    points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
    text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
      pos = 1, col = "blue")
    invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
      lwd = 3))
    st.name <- unique(cholera::roads[cholera::roads$street == road.number,
      "name"])
    title(main = paste0(st.name, ": 'Street' # ", road.number))

  } else {
    plot(cholera::fatalities[, c("x", "y")], xlim = x.rng, ylim = y.rng,
      pch = NA, asp = 1)
    invisible(lapply(roads.list, lines, col = "gray"))

    if (is.null(cases) == FALSE) {
      id <- cholera::road.segments[cholera::road.segments$street ==
        road.number, "id"]
      seg.ortho <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
        id, ]
      seg.anchors <- cholera::fatalities.address$anchor.case %in%
      seg.ortho$case
      seg.cases <- cholera::fatalities$case %in% seg.ortho$case

      if (cases == "all") {
        text(cholera::fatalities[!seg.cases, c("x", "y")],
         labels = cholera::fatalities$case[!seg.cases], cex = 0.5)
        if (any(seg.cases)) {
         text(cholera::fatalities[seg.cases, c("x", "y")],
           labels = cholera::fatalities$case[seg.cases], cex = 0.5, col = "red")
        }
      } else if (cases == "anchors") {
        text(cholera::fatalities.address[!seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[!seg.anchors],
           cex = 0.5)
        if (any(seg.anchors)) {
        text(cholera::fatalities.address[seg.anchors, c("x", "y")],
          labels = cholera::fatalities.address$anchor.case[seg.anchors],
          cex = 0.5, col = "red")
        }
      }
    }
  }

  points(cholera::pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
  text(cholera::pumps[, c("x", "y")], label = paste0("p", cholera::pumps$id),
   pos = 1)
  invisible(lapply(roads.list[paste(road.number)], lines, col = "red",
   lwd = 3))
  st.name <- unique(cholera::roads[cholera::roads$street == road.number,
   "name"])

   street.length <- cholera::streetLength(road.number, unit)

   if (is.null(unit)) {
     subtitle <- paste(round(street.length, 2), "units")
   } else if (unit == "meter") {
     subtitle <- paste(round(street.length, 2), "meters")
   } else if (unit == "yard") {
     subtitle <- paste(round(street.length, 2), "yards")
   }

   title(main = paste0(st.name, ": 'Street' # ", road.number), sub = subtitle)
}
