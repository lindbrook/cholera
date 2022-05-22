#' Partition fatalities (prototype).
#'
#' Minimize "adjacent" fatalities in stacks.
#' @return An R list.
#' @export

partitionFatalities <- function() {
  sel <- cholera::fatalities.address$case.count == 1
  single <- cholera::fatalities.address[sel, "anchor"]

  sel <- cholera::fatalities.address$case.count >= 2
  multiple <- cholera::fatalities.address[sel, "anchor"]

  multi.stacks <- lapply(multiple, function(x) {
    cholera::anchor.case[cholera::anchor.case$anchor == x, "case"]
  })

  names(multi.stacks) <- multiple

  multi.stacks.ct <- vapply(multi.stacks, length, integer(1L))
  st.ct <- sort(unique(multi.stacks.ct))
  st.ct <- st.ct[-1]

  stacks.stratified <- lapply(st.ct, function(x) {
    s.data <- multi.stacks[which(multi.stacks.ct == x)]
    strata <- lapply(seq_along(s.data), function(i) {
      dat <- s.data[[i]]
      out <- lapply(1:3, function(j) {
        idx <- seq(j, length(dat), 3)
        dat[idx]
      })
      names(out) <- paste(1:3)
      out
    })
    names(strata) <- names(s.data)
    strata
  })

  sample.select <- lapply(stacks.stratified, function(x) {
    randomized <- lapply(x, function(y) sample(y))
    one <-  unlist(lapply(randomized, function(x) x[[1]]))
    two <- unlist(lapply(randomized, function(x) x[[2]]))
    three <- unlist(lapply(randomized, function(x) x[[3]]))
    list(one = one, two = two, three = three)
  })

  set1 <- unlist(lapply(sample.select, function(x) x$one))
  set2 <- unlist(lapply(sample.select, function(x) x$two))
  set3 <- unlist(lapply(sample.select, function(x) x$three))

  # vapply(list(set1, set2, set3), length, integer(1L))

  # 2-member stacks #
  two.stack <- multi.stacks[which(multi.stacks.ct == 2)]
  two.stack.include <- round(length(two.stack) * 2 / 3)
  two.stack.leftover <- length(two.stack) * 2 - two.stack.include * 3
  two.data <- unlist(two.stack)
  two.idx <- 1:(two.stack.include * 3)
  two.transformed <- matrix(two.data[two.idx], ncol = 3, byrow = TRUE)

  sets <- lapply(1:3, function(i) {
    unname(c(get(paste0("set", i)), two.transformed[, i]))
  })

  one.data <- matrix(single, ncol = 3)
  sets <- lapply(1:3, function(i) c(sets[[i]], one.data[, i]))

  leftover.sel <- which.min(vapply(sets, length, integer(1L)))
  two.leftover <- two.data[setdiff(seq_along(two.data), two.idx)]

  sets[leftover.sel] <- list(c(sets[[leftover.sel]], unname(two.leftover)))
  sets
}


# plot(thresholdFataltiesGraph(), vertex.label = NA, vertex.size = 2)
