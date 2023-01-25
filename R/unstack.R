#' Unstack "stacks" in Snow's cholera map.
#'
#' Unstacks fatalities data by 1) assigning the coordinates of the base case to all cases in a stack and 2) setting the base case as an "address" and making the number of fatalities an attribute.
#' @param multi.core Logical or Numeric. \code{TRUE} uses \code{parallel::detectCores()}. \code{FALSE} uses one, single core. With Numeric, you specify the number logical cores. See \code{vignette("Parallelization")} for details.
#' @param compute Logical. \code{TRUE} computes data. \code{FALSE} uses pre-computed data.
#' @param dev.mode Logical. Development mode uses parallel::parLapply().
#' @seealso \code{vignette("unstacking.fatalities")}
#' @return An R list that includes \code{anchor.case}, \code{fatalities.address}, \code{fatalities.unstacked} and \code{ortho.proj}.
#' @note This function is computationally intensive. This function documents the code that generates \code{\link{anchor.case}},  \code{\link{fatalities.address}}, \code{\link{fatalities.unstacked}} and \code{\link{ortho.proj}}.
#' @export

unstackFatalities <- function(multi.core = TRUE, compute = FALSE,
  dev.mode = FALSE) {

  if (compute) {
    cores <- multiCore(multi.core)

    rd <- cholera::roads[!cholera::roads$street %in% cholera::border, ]
    map.frame <- cholera::roads[cholera::roads$street %in% cholera::border, ]
    roads.list <- split(rd[, c("x", "y")], rd$street)
    border.list <- split(map.frame[, c("x", "y")], map.frame$street)

    fixed.fatalities <- fixFatalities()

    orthogonal.projection <- orthoProj(fixed.fatalities, fixed.fatalities$case,
      cores, dev.mode)
    ortho.proj <- do.call(rbind, orthogonal.projection)
    row.names(ortho.proj) <- NULL
    ortho.proj$case <- fixed.fatalities$case

    # classification errors due to bar orientation
    road.segment.fix <- roadSegmentFix()

    # Recompute orthogonal distances
    ortho.projB <- orthoProjB(fixed.fatalities, road.segment.fix, cores,
      dev.mode)
    ortho.projB <- do.call(rbind, ortho.projB)
    row.names(ortho.projB) <- NULL

    ## Manual fix: segment endpoint cases ##

    ## Portland Mews: 286, 558 ##

    #       56
    # 558  286  anchor
    # --------

    old.st <- "160-2"
    new.st <- "160-3"

    case.select <- 286
    case <- fixed.fatalities[case.select, ]
    x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
    y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
    ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
    data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
      ortho.dist = c(ortho.dist), case = case.select)
    ortho.projB <- rbind(ortho.projB, data.fix)

    case.select <- 56
    case <- fixed.fatalities[case.select, ]
    x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
    y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
    ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
    data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
      ortho.dist = c(ortho.dist), case = case.select)
    ortho.projB <- rbind(ortho.projB, data.fix)


    ## Use orthogonal projection instead of segment endpoint
    # stack440 <- c(440, 480, 500, 571, 444, 497, 145) # @ "259-1"
    ## William and Mary Yard: 440 ##
    # ortho.proj[ortho.proj$case == 440, ]

    # 259-2: |
    # 259-1:  _

    # old.st <- "317-3"
    # new.st <- "259-1"

    # case.select <- 440
    # case <- fixed.fatalities[case.select, ]
    # x.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "x2"]
    # y.proj <- cholera::road.segments[cholera::road.segments$id == new.st, "y2"]
    # ortho.dist <- stats::dist(rbind(case[, c("x", "y")], c(x.proj, y.proj)))
    # data.fix <- data.frame(road.segment = new.st, x.proj, y.proj,
    #   ortho.dist = c(ortho.dist), case = case.select)
    # ortho.projB <- rbind(ortho.projB, data.fix)

    ## St James Workhouse: [369] 434, 11, 53, 193
    # move anchor 369 and associated cases to intersection of Poland Street and
    # endpoint of St James Workhouse segment. Use Poland Street address!

    pl.st <- "194-1"
    case.select <- c(369, 434, 11, 53, 193)
    case <- fixed.fatalities[fixed.fatalities$case %in% case.select, ]
    x.proj <- cholera::road.segments[cholera::road.segments$id == pl.st, "x1"]
    y.proj <- cholera::road.segments[cholera::road.segments$id == pl.st, "y1"]
    eucl.dist <- vapply(case$case, function(x) {
      stats::dist(rbind(case[case$case == x, c("x", "y")], c(x.proj, y.proj)))
    }, numeric(1L))
    data.fix <- data.frame(road.segment = pl.st, x.proj, y.proj,
      ortho.dist = eucl.dist, case = case$case)
    ortho.projB <- rbind(ortho.projB, data.fix)

    ## Re-assemble ##

    ortho.proj[ortho.proj$case %in% ortho.projB$case, ] <- ortho.projB
    ortho.proj <- ortho.proj[order(ortho.proj$case), ]
    row.names(ortho.proj) <- NULL

    ## Single and Multiple ##

    road.incidence <- table(ortho.proj$road.segment)
    road.incidence <- data.frame(id = names(road.incidence),
      count = c(road.incidence), stringsAsFactors = FALSE)
    row.names(road.incidence) <- NULL

    single.obs <- road.incidence[road.incidence$count == 1, ]
    single.address <- lapply(single.obs$id, function(i) {
      data.frame(id = i, case = ortho.proj[ortho.proj$road.segment == i,
        "case"])
    })

    cutpoint <- 0.05
    multiple.obs <- road.incidence[road.incidence$count > 1, ]
    multiple.address <- multipleAddress(multiple.obs, ortho.proj,
      fixed.fatalities, cutpoint, cores, dev.mode)

    multiple.unstacked <- multipleUnstack(multiple.address, cores, dev.mode)
    multiple.unstacked <- do.call(rbind,multiple.unstacked)
    multiple.unstacked$multiple.obs.seg <- "Yes"

    single.unstacked <- do.call(rbind, single.address)
    single.unstacked[ c("group", "anchor",  "case.count")] <- 1
    single.unstacked$anchor <- single.unstacked$case
    single.unstacked$multiple.obs.seg <- "No"

    unstacked <- rbind(multiple.unstacked, single.unstacked)
    unstacked <- merge(unstacked, fixed.fatalities, by.x = "anchor",
      by.y = "case")

    fatalities.unstacked <- unstacked[, c("case", "x", "y")]
    idx <- order(fatalities.unstacked$case)
    fatalities.unstacked <- fatalities.unstacked[idx, ]
    row.names(fatalities.unstacked) <- NULL

    vars <- c("case", "x", "y", "case.count")
    fatalities.address <- unstacked[unstacked$anchor == unstacked$case, vars]
    names(fatalities.address)[1] <- "anchor"
    row.names(fatalities.address) <- NULL

    anchor.case <- unstacked[, c("anchor", "case")]

    list(anchor.case = anchor.case,
         fatalities.unstacked = fatalities.unstacked,
         fatalities.address = fatalities.address,
         ortho.proj = ortho.proj)

    ## output tests ##

    # TRUE
    # identical(anchor.case, cholera::anchor.case)

    # TRUE
    # vars <- c("case", "x", "y")
    # identical(fatalities.unstacked, cholera::fatalities.unstacked[, vars])

    # TRUE
    # vars <- c("anchor", "x", "y", "case.count")
    # identical(fatalities.address, cholera::fatalities.address[, vars])

    # FALSE
    # identical(ortho.proj, cholera::ortho.proj)
    # identical(ortho.proj$road.segment, cholera::ortho.proj$road.segment)
    # identical(ortho.proj$case, cholera::ortho.proj$case)
    # identical(ortho.proj$x.proj, cholera::ortho.proj$x.proj)
    # identical(ortho.proj$y.proj, cholera::ortho.proj$y.proj)
    # ortho.proj[ortho.proj$x.proj != cholera::ortho.proj$x.proj, ]
    # cholera::ortho.proj[ortho.proj$x.proj != cholera::ortho.proj$x.proj, ]
    # usethis::use_data(ortho.proj, overwrite = TRUE)

  } else {
    list(anchor.case = cholera::anchor.case,
        fatalities.address = cholera::fatalities.address,
        fatalities.unstacked = cholera::fatalities.unstacked,
        ortho.proj = cholera::ortho.proj)
  }
}
