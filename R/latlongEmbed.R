#' Embed anchors and pumps into road segments (prototype).
#'
#' @param vestry Logical.
#' @param case.set Character. "observed" or "expected".
#' @param embed.addr Logical. Embed case address into graph network.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. You can also specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @noRd

latlongEmbed <- function(vestry = FALSE, case.set = "observed",
  embed.addr = TRUE, multi.core = FALSE) {

  cores <- multiCore(multi.core)

  if (embed.addr) {
    if (case.set == "observed") {
      ortho.addr <- cholera::latlong.ortho.addr
    } else if (case.set == "expected") {
      ortho.addr <- cholera::latlong.sim.ortho.proj
    }
  }

  ## embed just pumps v. embed cases and pumps

  if (vestry) ortho.pump <- cholera::latlong.ortho.pump.vestry
  else ortho.pump <- cholera::latlong.ortho.pump
  names(ortho.pump)[names(ortho.pump) == "id"] <- "pump"

  road.data <- roadSegments(latlong = TRUE)

  if (embed.addr) {
    obs.segs <- union(ortho.addr$road.segment, ortho.pump$road.segment)
  } else {
    obs.segs <- ortho.pump$road.segment
  }

  no_embeds <- road.data[!road.data$id %in% obs.segs, ]

  vars <- c("lon", "lat")
  vars2 <- c(vars, "case", "pump")

  embeds <- parallel::mclapply(obs.segs, function(s) {
    rd.tmp <- road.data[road.data$id == s, ]
    pump.tmp <- ortho.pump[ortho.pump$road.segment == s, ]
    endpts <- data.frame(lon = unlist(rd.tmp[, paste0(vars[1], 1:2)]),
                         lat = unlist(rd.tmp[, paste0(vars[2], 1:2)]),
                         case = 0,
                         pump = 0,
                         row.names = NULL)

    if (nrow(pump.tmp) > 0) {
      pump.tmp$case <- 0L
      pump.embed <- pump.tmp[, c(vars, "case", "pump")]
    }

    if (embed.addr) {
      addr.tmp <- ortho.addr[ortho.addr$road.segment == s, ]
      if (nrow(addr.tmp) > 0) addr.embed <- addr.tmp[, c(vars, "case")]
      
      if (nrow(addr.tmp) > 0 & nrow(pump.tmp) > 0) {
        addr.embed$pump <- 0
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, addr.embed, pump.embed)
      } else if (nrow(addr.tmp) == 0 & nrow(pump.tmp) > 0) {
        pump.embed$case <- 0
        pump.embed <- pump.embed[, vars2]
        embed.data <- rbind(endpts, pump.embed)
      } else if (nrow(addr.tmp) > 0 & nrow(pump.tmp) == 0) {
        addr.embed$pump <- 0
        embed.data <- rbind(endpts, addr.embed)
      }
    } else {
      embed.data <- rbind(endpts, pump.embed)
    }

    nodes <- embed.data[order(embed.data$lon, embed.data$lat), ]
    tmp <- nodes[, vars]
    tmp <- cbind(tmp[-nrow(tmp), ], tmp[-1, ])
    coord.nms <- paste0(names(tmp), c(rep(1, 2), rep(2, 2)))
    names(tmp) <- coord.nms
    tmp <- cbind(tmp, rd.tmp[, c("street", "id", "name")])
    edges <- tmp[, c("street", "id", "name", coord.nms)]

    if (case.set == "observed") {
      edges$id2 <- paste0(edges$id, letters[seq_len(nrow(edges))])
    } else if (case.set == "expected") {
      edges$id2 <- paste0(edges$id, "-", seq_len(nrow(edges)))
    }

    list(edges = edges, nodes = nodes, road.data = road.data)
  }, mc.cores = cores)

  no_embeds$id2 <- paste0(no_embeds$id, "a")

  edges <- do.call(rbind, lapply(embeds, function(x) x$edges))
  edges <- rbind(edges, no_embeds[, names(edges)])
  edges <- edges[order(edges$street), ]
  nodes <- do.call(rbind, lapply(embeds, function(x) x$nodes))
  list(edges = edges, nodes = nodes, road.data = road.data)
}
