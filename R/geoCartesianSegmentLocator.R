#' Locate road by name on geo-cartesian map.
#'
#' @param street.name Character. Vector of street names.
#' @param street.col Character. Color of streets.
#' @param zoom Logical.
#' @param zoom.padding Numeric. Padding in "meters".
#' @return A base R graphics plot.
#' @noRd

geoCartesianStreetLocator <- function(street.name = NULL, street.col = "gray",
  zoom = TRUE, zoom.padding = 0) {

  real.road.names <- streetNames()

  if (!is.null(street.name)) {
    st.nm <- vapply(street.name, caseAndSpace, character(1L))

    if (all(st.nm %in% real.road.names == FALSE)) {
      error.msg <- "Invalid street name(s). See streetNames()."
      stop(error.msg, call. = FALSE)
    } else if (any(st.nm %in% real.road.names == FALSE)) {
      nm.err <- st.nm[!st.nm %in% real.road.names]
      nm.msg <- "Misspelled or invalid street name(s). See streetNames():"
      message(paste(nm.msg, paste(nm.err, collapse = ", ")))
      st.nm <- st.nm[st.nm %in% real.road.names]
    }
  }

  rd <- cholera::roads[!cholera::roads$street %in% cholera::border, ]
  cartesian.rd <- data.frame(street = rd$street, geoCartesian(rd))
  vars <- c("x", "y")

  roads.list <- split(cartesian.rd[, vars], cartesian.rd$street)

  if (is.null(street.name) | isFALSE(zoom)) {
    xlim <- NULL
    ylim <- NULL
  } else if (!is.null(street.name) & isTRUE(zoom)) {
    sel <- cartesian.rd$street %in% unique(rd[rd$name %in% st.nm, "street"])

    if (zoom.padding > 0) {
      pad <- c(-zoom.padding, zoom.padding)
      xlim <- range(cartesian.rd[sel, "x"]) + pad
      ylim <- range(cartesian.rd[sel, "y"]) + pad
    } else if (zoom.padding == 0) {
      xlim <- range(cartesian.rd[sel, "x"])
      ylim <- range(cartesian.rd[sel, "y"])
    } else stop("zoom.padding must be >= 0.")
  }

  plot(cartesian.rd[, vars], asp = 1, pch = NA, xlim = xlim, ylim = ylim)
  invisible(lapply(roads.list, lines, col = street.col))
}
