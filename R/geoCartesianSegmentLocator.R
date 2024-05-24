#' Locate road by name on geo-cartesian map.
#'
#' @param street.name Character. Vector of street names.
#' @param zoom Logical.
#' @return A base R graphics plot.
#' @noRd

geoCartesianStreetLocator <- function(street.name = NULL, zoom = TRUE) {
  if (!is.null(street.name)) {
    st.nm <- vapply(street.name, caseAndSpace, character(1L))
  }

  rd <- cholera::roads[!cholera::roads$street %in% cholera::border, ]
  cartesian.rd <- data.frame(street = rd$street, geoCartesian(rd))
  vars <- c("x", "y")

  cartesian.rd.segs <- lapply(unique(cartesian.rd$street), function(st) {
    dat <- cartesian.rd[cartesian.rd$street == st, ]
    names(dat)[names(dat) %in% vars] <- paste0(vars, 1)
    seg.data <- dat[-1, paste0(vars, 1)]
    names(seg.data) <- paste0(vars, 2)
    dat <- cbind(dat[-nrow(dat), ], seg.data)
    dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
    dat
  })

  cartesian.rd.segs <- do.call(rbind, cartesian.rd.segs)

  if (is.null(street.name) | isFALSE(zoom)) {
    xlim <- NULL
    ylim <- NULL
  } else {
    sel <- cartesian.rd$street %in% unique(rd[rd$name %in% st.nm, "street"])
    xlim <- range(cartesian.rd[sel, "x"])
    ylim <- range(cartesian.rd[sel, "y"])
  }

  plot(cartesian.rd[, vars], asp = 1, pch = NA, xlim = xlim, ylim = ylim)
  idx <- seq_len(nrow(cartesian.rd.segs))
  invisible(lapply(idx, function(i) {
    st.seg <- cartesian.rd.segs[i, ]
    segments(st.seg$x1, st.seg$y1, st.seg$x2, st.seg$y2)
  }))
}
