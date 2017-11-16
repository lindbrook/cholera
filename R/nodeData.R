#' Embed cases ("addresses") and pumps into road segment.
#'
#' @param id Character. Road segment ID.
#' @param type Character. Type of output: nodes or edge list.
#' @param vestry Logical. Use Vestry Report pump data.
#' @return An R data frame.
#' @export
#' @examples
#' nodeData("216-1")
#' nodeData("216-1", "edges")
#' nodeData()

nodeData <- function(id = "242-1", type = "nodes", vestry = FALSE) {
  if (id %in% cholera::road.segments$id == FALSE) {
      stop('Valid "id" are listed in cholera::road.segments$id.')
  }

  road.data <- cholera::road.segments[cholera::road.segments$id == id, ]

  if (road.data$id %in% cholera::ortho.proj$road.segment) {
    road.fatalities <- cholera::ortho.proj[cholera::ortho.proj$road.segment %in%
      road.data$id, ]

    sel <- road.fatalities$case[road.fatalities$case %in%
      cholera::fatalities.address$anchor.case]
    road.address <- road.fatalities[road.fatalities$case %in% sel, ]

    rds <- data.frame(road.address[, c("x.proj", "y.proj")],
                      anchor = road.address$case,
                      pump = 0)
  }

  endptA <- data.frame(x.proj = road.data$x1,
                       y.proj = road.data$y1,
                       anchor = 0,
                       pump = 0)

  endptB <- data.frame(x.proj = road.data$x2,
                       y.proj = road.data$y2,
                       anchor = 0,
                       pump = 0)

  if (vestry) {
    pumps <- cholera::ortho.proj.pump.vestry
  } else {
    pumps <- cholera::ortho.proj.pump
  }

  case.seg <- road.data$id %in% cholera::ortho.proj$road.segment
  pump.seg <- id %in% pumps$road.segment

  if (pump.seg) {
    pump.data <- pumps[pumps$road.segment == id, ]
    ps <- data.frame(pump.data[, c("x.proj", "y.proj")], anchor = 0,
      pump = pump.data$pump.id)

    if (case.seg) {
      dat <- rbind(rds, ps)
      nodes <- rbind(endptA, dat[order(dat$x.proj), ], endptB)
    } else {
      nodes <- rbind(endptA, ps, endptB)
    }
  } else nodes <- rbind(endptA, rds[order(rds$x.proj), ], endptB)

  row.names(nodes) <- NULL
  nodes$node <- paste0(nodes$x.proj, "-", nodes$y.proj)

  # Edges

  sel <- c("x.proj", "y.proj")
  edges <- cbind(nodes[-nrow(nodes), sel], nodes[-1, sel])
  names(edges) <- c("x1", "y1", "x2", "y2")

  edges$node1 <- paste0(edges$x1, "-", edges$y1)
  edges$node2 <- paste0(edges$x2, "-", edges$y2)

  edges <- cbind(road.data[1, c("street", "id", "name")], edges,
    row.names = NULL)

  edges$id <- paste0(edges$id, letters[seq_len(nrow(edges))])

  if (type == "nodes") {
    nodes
  } else if (type == "edges") {
    edges
  }
}
