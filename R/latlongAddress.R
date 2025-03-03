#' Compute latitude and longitude of case "addresses" (prototype).
#'
#' @param path Character. e.g., "~/Documents/Data/"
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @return An R data frame.
#' @noRd
#' @note This documents the computation of the latlong version of the fatalities.address data frame.

latlongAddress <- function(path, multi.core = FALSE) {
  # reset (delete) lon-lat for recomputation
  sel <- !names(cholera::fatalities) %in% c("lon", "lat")
  fatalities.original <- cholera::fatalities[, sel]
  sel <- !names(cholera::fatalities.address) %in% c("lon", "lat")
  address.original <- cholera::fatalities.address[, sel]

  pre <- paste0(path, "address.v")
  post <- "_modified.tif"
  cores <- multiCore(multi.core)

  addr.partitions <- partitionAddresses()

  # values of k for stats::hclust()
  ks <- vapply(addr.partitions, length, integer(1L))

  coords <- parallel::mclapply(seq_along(addr.partitions), function(i) {
    tif <- paste0(pre, i, post)
    k <- ks[i]
    latlongCoordinates(tif, k, path)
  }, mc.cores = cores)

  stop <- cumsum(ks)
  start <- c(1, stop[-length(stop)] + 1)
  k.idx <- data.frame(start = start, stop = stop, row.names = NULL)

  coords <- lapply(seq_along(coords), function(i) {
    tmp <- coords[[i]]
    tmp$id <- k.idx[i, "start"]:k.idx[i, "stop"]
    tmp
  })

  address.groups <- lapply(addr.partitions, function(case) {
    fatalities.original[fatalities.original$case %in% case, ]
  })

  address.rotate.scale <- parallel::mclapply(address.groups, function(x) {
    tmp <- lapply(x$case, function(y) rotatePoint(y, dataset = "fatalities"))
    tmp <- do.call(rbind, tmp)
    data.frame(id = x$case, scale(tmp))
  }, mc.cores = cores)

  coords.scale <- lapply(coords, function(x){
    data.frame(id = x$id, scale(x[, c("lon", "lat")]))
  })

  match.points <- parallel::mclapply(seq_along(coords.scale), function(i) {
    addr <- address.rotate.scale[[i]]
    alters <- coords.scale[[i]]
    names(alters)[-1] <- c("x", "y")
    out <- lapply(addr$id, function(id) {
      ego <- addr[addr$id == id, c("x", "y")]
      d <- vapply(seq_len(nrow(alters)), function(i) {
        stats::dist(rbind(ego, alters[i, c("x", "y")]))
      }, numeric(1L))
      data.frame(id = id, geo.id = alters$id[which.min(d)])
    })
    do.call(rbind, out)
  }, mc.cores = cores)

  match.points <- do.call(rbind, match.points)
  coords <- do.call(rbind, coords)

  out <- merge(address.original, match.points, by.x = "anchor", by.y = "id")
  out <- merge(out, coords, by.x = "geo.id", by.y = "id")
  out <- out[order(out$anchor), ]
  out$geo.id <- NULL
  row.names(out) <- NULL
  out
}

#' Compute latitude and longitude version fatalities.unstacked.
#'
#' @return An R data frame.
#' @noRd
#' @note This documents the computation of the lat-long version of the fatalities.unstacked data frame.

latlongFatalitiesUnstacked <- function() {
  sel.rows <- cholera::fatalities.address$case.count == 1
  sel.vars <- names(cholera::fatalities.address) != "case.count"
  single.stack <- cholera::fatalities.address[sel.rows, sel.vars]
  names(single.stack)[1] <- "case"

  sel.rows <- cholera::fatalities.address$case.count > 1
  multi.stack <- cholera::fatalities.address[sel.rows, sel.vars]

  multiples <- lapply(multi.stack$anchor, function(a) {
    tmp <- cholera::fatalities
    vars <- !names(cholera::fatalities.unstacked) %in% c("lon", "lat")
    tmp.unstacked <- cholera::fatalities.unstacked[, vars]
    stack <- cholera::anchor.case[cholera::anchor.case$anchor == a, "case"]
    anchor.coords <- tmp[tmp$case == a, c("lon", "lat")]
    cbind(tmp.unstacked[tmp.unstacked$case %in% stack, ], anchor.coords)
  })

  out <- rbind(single.stack, do.call(rbind, multiples))
  out <- out[order(out$case), ]
  row.names(out) <- NULL
  out
}

fatalitiesUnstack <- function() {
  census <- table(cholera::anchor.case$anchor)
  multiple <- names(census[census != 1])
  single <- names(census[census == 1])
  vars <- c("x", "y", "lon", "lat")

  multi.data <- lapply(multiple, function(anchor) {
    stack <- cholera::anchor.case[cholera::anchor.case$anchor == anchor, "case"]
    others <- setdiff(stack, anchor)
    coords <- cholera::fatalities[cholera::fatalities$case == anchor, vars]
    tmp <- cholera::fatalities[cholera::fatalities$case %in% stack, ]
    tmp[tmp$case %in% others, vars] <- coords
    tmp
  })

  multi.data <- do.call(rbind, multi.data)
  single.data <- cholera::fatalities[cholera::fatalities$case %in% single, ]
  out <- rbind(multi.data, single.data)
  out[order(out$case), ]
}

# fatalities.unstacked <- fatalitiesUnstack()

