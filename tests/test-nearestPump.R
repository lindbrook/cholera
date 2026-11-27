library(cholera)

pf_67_walk <- pumpFatalities(pump.select = 6:7, latlong = TRUE)
stopifnot(
  identical(names(pf_67_walk), c("6", "7")),
  sum(pf_67_walk) == 578L
)

pf_m7_walk <- pumpFatalities(pump.select = -7, latlong = TRUE)
stopifnot(
  !("7" %in% names(pf_m7_walk)),
  sum(pf_m7_walk) == 578L
)

pf_67_eucl <- pumpFatalities(
  pump.select = 6:7,
  metric = "euclidean",
  latlong = TRUE
)
stopifnot(
  identical(names(pf_67_eucl), c("6", "7")),
  sum(pf_67_eucl) == 578L
)

nw_67 <- neighborhoodWalking(pump.select = 6:7, latlong = TRUE)
np_67 <- cholera:::nearestPump(pump.select = 6:7, latlong = TRUE)
stopifnot(
  identical(sort(np_67$case[np_67$pump == 6]), sort(nw_67$case.pump$p6)),
  identical(sort(np_67$case[np_67$pump == 7]), sort(nw_67$case.pump$p7))
)
