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

#' Create and write GeoPackage (GPKG) of roads data (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd

roadGPKG <- function(path) {
  vars <- c("x", "y")
  dat <- cholera::roads[, vars]
  road_geom <- sf::st_as_sf(dat, coords = vars)
  road_attr <- cholera::roads[, c("id", "street", "n", "name")] 
  road_sf <- sf::st_sf(road_attr, geometry = sf::st_as_sfc(road_geom))
  sf::write_sf(road_sf, paste0(path, "road.gpkg"), append = FALSE)
}

#' Create and write GeoPackage (GPKG) of road line segment network (prototype).
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @noRd
#' @note For georeferencing.

roadSegmentGPKG <- function(path) {
  vars <- c("x1", "y1", "x2", "y2")
  seg.data <- lapply(seq_along(cholera::road.segments$id), function(i) {
    matrix(unlist(cholera::road.segments[i, vars]), ncol = 2, byrow = TRUE)
  })
  rd.segs_geom <- lapply(seg.data, sf::st_linestring)
  rd.segs_attr <- cholera::road.segments[, c("street", "id", "name")]
  rd.segs_sf <- sf::st_sf(rd.segs_attr, geometry = sf::st_sfc(rd.segs_geom))
  sf::st_write(rd.segs_sf, paste0(path, "roadSegment.gpkg"), append = FALSE)
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

#' Extract Longitude and Latitude from Georeferenced GeoPackage.
#'
#' @param path Character. File path e.g., "~/Documents/Data/".
#' @param dataset Character. Name of 'cholera' dataset.
#' @noRd

latlongCoordinatesGPKG <- function(path, dataset = "fatalities") {
  if (dataset == "fatalities") {
    dat <- "fatality_modified.gpkg"
    nom.data <- cholera::fatalities
  } else if (dataset == "fatalities.address") {
    dat <- "anchor_modified.gpkg"
    nom.data <- cholera::fatalities.address
  } else if (dataset == "fatalities.unstacked") {
    dat <- "unstack_modified.gpkg"
    nom.data <- cholera::fatalities.unstacked
  } else if (dataset == "pumps") {
    dat <- "pump_modified.gpkg"
    nom.data <- cholera::pumps
  } else if (dataset == "pumps.vestry") {
    dat <- "pumpVestry_modified.gpkg"
    nom.data <- cholera::pumps.vestry
  } else if (dataset == "roads") {
    dat <- "road_modified.gpkg"
    nom.data <- cholera::roads
  } else if (dataset == "frame.data") {
    dat <- "frame_modified.gpkg"
    nom.data <- cholera::frame.data
  } else if (dataset == "road.segments") {
    dat <- "roadSegment_modified.gpkg"
    nom.data <- cholera::road.segments
  } else {
    stop('Invalid dataset. Check spelling.', call. = FALSE)
  }

  geo.data <- sf::st_read(paste0(path, dat), quiet = TRUE)
  
  if (!dataset %in% c("frame.data", "road.segments")) {
    vars <- c("lon", "lat")
    if (all(vars %in% names(nom.data))) {
      nom.data <- nom.data[, !names(nom.data) %in% vars]
    }
    geo.data <- stats::setNames(data.frame(sf::st_coordinates(geo.data)), vars)
  } else {
    vars <- c("lon1", "lat1", "lon2", "lat2")
    if (all(vars %in% names(nom.data))) {
      nom.data <- nom.data[, !names(nom.data) %in% vars]
    }
    st <- data.frame(sf::st_coordinates(geo.data))
    geo.data <- do.call(rbind, lapply(unique(st$L1), function(id) {
      tmp <- st[st$L1 == id, ]
      tmp <- cbind(tmp[1, c("X", "Y")], tmp[2, c("X", "Y")])
      names(tmp) <- vars
      tmp
    }))
  }
  cbind(nom.data, geo.data)
}

#' @importFrom sf st_as_sf st_as_sfc st_coordinates st_read st_sf write_sf
