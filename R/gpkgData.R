#' Create and write GeoPackage (GPKG) of anchor fatalities (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd

anchorsGPKG <- function(path) {
  vars <- c("x", "y")
  dat <- cholera::fatalities.address[, vars]
  anchor_geom <- sf::st_as_sf(dat, coords = vars)
  anchor_attr <- cholera::fatalities.address[, c("anchor", "case.count")]
  anchor_sf <- sf::st_sf(anchor_attr, geometry = sf::st_as_sfc(anchor_geom))
  sf::write_sf(anchor_sf, paste0(path, "anchor.gpkg"), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of fatalities (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd

fatalitiesGPKG <- function(path) {
  vars <- c("x", "y")
  dat <- cholera::fatalities[, vars]
  fatality_geom <- sf::st_as_sf(dat, coords = vars)
  fatality_attr <- cholera::fatalities[, "case", drop = FALSE]
  fatality_sf <- sf::st_sf(fatality_attr, 
    geometry = sf::st_as_sfc(fatality_geom))
  sf::write_sf(fatality_sf, paste0(path, "fatality.gpkg"), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of fatalities.unstacked (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @noRd

unstackedGPKG <- function(path) {
  vars <- c("x", "y")
  dat <- cholera::fatalities.unstacked[, vars]
  unstack_geom <- sf::st_as_sf(dat, coords = vars)
  unstack_attr <- cholera::fatalities.unstacked[, "case", drop = FALSE]
  unstack_sf <- sf::st_sf(unstack_attr, geometry = sf::st_as_sfc(unstack_geom))
  sf::write_sf(unstack_sf, paste0(path, "unstack.gpkg"), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of pump data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param vestry Logical.
#' @noRd

pumpsGPKG <- function(path, vestry = FALSE) {
  vars <- c("x", "y")
  if (vestry) {
    dat <- cholera::pumps.vestry[, vars]
    pump_attr <- cholera::pumps.vestry[, c("id", "street")]
    file.nm <- "pumpVestry.gpkg"
  } else {
    dat <- cholera::pumps[, vars]
    pump_attr <- cholera::pumps[, c("id", "street")]
    file.nm <- "pump.gpkg"
  }
  pump_geom <- sf::st_as_sf(dat, coords = vars)
  pump_sf <- sf::st_sf(pump_attr, geometry = sf::st_as_sfc(pump_geom))
  sf::write_sf(pump_sf, paste0(path, file.nm), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of road line segment network (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd
#' @note For georeferencing.

roadsGPKG <- function(path) {
  vars <- c("x1", "y1", "x2", "y2")
  seg.data <- lapply(seq_along(cholera::road.segments$id), function(i) {
    matrix(unlist(cholera::road.segments[i, vars]), ncol = 2, byrow = TRUE)
  })
  rds_geom <- lapply(seg.data, sf::st_linestring)
  rds_attr <- cholera::road.segments[, c("street", "id", "name")]
  rds_sf <- sf::st_sf(rds_attr, geometry = sf::st_sfc(rds_geom))
  sf::st_write(rds_sf, paste0(path, "roads.gpkg"), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of map frame (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd

mapFrameGPKG <- function(path) {
  vars <- c("x", "y")
  frm <- cholera::roads[cholera::roads$name == "Map Frame", ]
  frame.data <- lapply(frm$street, function(s) {
    matrix(unlist(frm[frm$street == s, vars]), ncol = 2, byrow = FALSE)
  })
  frame_geom <- lapply(frame.data, sf::st_linestring)
  vars2 <-  c("id", "street", "n")
  frame_attr <- cholera::roads[cholera::roads$name == "Map Frame", vars2]
  frame_sf <- sf::st_sf(frame_attr, geometry = sf::st_sfc(frame_geom))
  sf::st_write(frame_sf, paste0(path, "frame.gpkg"), append = FALSE)
}

#' @importFrom sf st_as_sf st_as_sfc st_coordinates st_read st_sf write_sf