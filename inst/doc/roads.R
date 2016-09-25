## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cholera)

## ---- fig.width = 6, fig.height = 6, fig.align = "center", echo = FALSE----

roads.list <- split(roads[, c("x", "y")], roads$street)

plot(fatalities[, c("x", "y")], xlim = range(roads$x), ylim = range(roads$y),
     pch = 15, cex = 0.5, col = "gray", asp = 1)
invisible(lapply(roads.list, lines, col = "gray"))
points(HistData::Snow.pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")
text(HistData::Snow.pumps[, c("x", "y")], labels = HistData::Snow.pumps$pump,
     pos = 1)

## ------------------------------------------------------------------------
head(roads)
nrow(roads)
length(unique(roads$street))


## ------------------------------------------------------------------------
# Map Border "Streets" ##

top <- c(1:12, 14)
right <- c(37, 62, 74, 142, 147, 205, 240, 248, 280, 360, 405, 419, 465)
bottom <- c(483, seq(487, 495, 2), 498, 500, seq(503, 519, 2))
left <- c(31, 79, 114, 285, 348, 397, 469)
border <- sort(c(bottom, left, top, right))

length(border)


## ---- eval = FALSE-------------------------------------------------------
#  snow.streets <- HistData::Snow.streets
#  snow.streets$id <- seq_len(nrow(snow.streets))
#  
#  # Data frame of road names
#  road.data <- read.csv("~/Documents/Data IV/Snow/road3b.csv",
#                        stringsAsFactors = FALSE)
#  
#  roads <- merge(snow.streets, road.data, by = "street", all.x = TRUE)
#  roads[is.na(roads$name), "name"] <- "Map Frame"
#  
#  roads[roads$id == 277, "street"] <- 116
#  roads[roads$id == 277, "name"] <- "Marlborough Mews"
#  roads[roads$id == 277, c("x", "y")] <- roads[roads$id == 276, c("x", "y")]
#  roads[roads$name == "Queen Street (I)", "n"] <- 4
#  roads[roads$name == "Marlborough Mews", "n"] <- 3
#  roads <- roads[order(roads$id), ]

## ---- fig.width = 6, fig.height = 6, fig.align = "center", echo = TRUE----
streetNameViewer("Oxford Street")
streetNameViewer("Cambridge Street", zoom = TRUE, radius = 0.5)
streetNumberViewer(50)

