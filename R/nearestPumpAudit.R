nearestPumpAudit <- function(vestry = FALSE, case.set = "observed",
  weighted = TRUE, multi.core = FALSE, dev.mode = FALSE) {

  cores <- multiCore(multi.core)
  dat <- neighborhoodData(vestry = vestry, case.set = case.set)
  # path.data <- pathData(dat, weighted, case.set, cores, dev.mode)

  ## ----- ##

  g <- dat$g
  nodes <- dat$nodes
  edges <- dat$edges
  nodes.pump <- dat$nodes.pump

  ## Adam and Eve Court: isolate with pump ##
  rd <- "Adam and Eve Court"
  adam.eve.ct <- cholera::road.segments[cholera::road.segments$name == rd, "id"]
  sel <- cholera::sim.ortho.proj$road.segment == adam.eve.ct &
         !is.na(cholera::sim.ortho.proj$road.segment)
  AE.cases <- cholera::sim.ortho.proj[sel, "case"]

  ## Falconberg Court and Mews: isolate without pumps ##
  falconberg.ct.mews <- c("40-1", "41-1", "41-2", "63-1")
  sel <- cholera::sim.ortho.proj$road.segment %in% falconberg.ct.mews &
         !is.na(cholera::sim.ortho.proj$road.segment)
  FCM.cases <- cholera::sim.ortho.proj[sel, "case"]

  # dists <- parallel::mclapply(x, function(a) {

  case <- nodes[nodes$anchor != 0 & nodes$anchor < 20000, "anchor"]
  # all(sort(case) == seq_len(nrow(regular.cases)))

  exp.case <- case[case %in% FCM.cases == FALSE]

  # nearest.pump <- vector(mode = "numeric", length = length(exp.case))
  nearest.pump <- matrix(0, nrow = length(exp.case), ncol = 3)

  for (i in seq_along(exp.case)) {
    case.node <- nodes[nodes$anchor == exp.case[i], "node"]
    d <- c(igraph::distances(g, case.node, nodes.pump$node,
      weights = edges$d))
    names(d) <- nodes.pump$pump
    p <- as.numeric(names(which.min(d[is.infinite(d) == FALSE])))
    # cat(exp.case[i], "")
    nearest.pump[i, ] <- c(exp.case[i], min(d[is.infinite(d) == FALSE]), p)
  }

  stats::setNames(as.data.frame(nearest.pump), c("case", "distance", "pump"))
}
