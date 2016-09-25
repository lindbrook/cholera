## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library("cholera")
library("HistData")
library("ggplot2")
library("KernSmooth")

bandwidth <- 0.5

top <- c(1:12, 14)
right <- c(37, 62, 74, 142, 147, 205, 240, 248, 280, 360, 405, 419, 465)
bottom <- c(483, seq(487, 495, 2), 498, 500, seq(503, 519, 2))
left <- c(31, 79, 114, 285, 348, 397, 469)
border <- sort(c(bottom, left, top, right))

map.border <- Snow.streets[Snow.streets$street %in% border == TRUE, ]
border.list <- split(map.border[, c("x", "y")], map.border$street)

## Roads ##

roads.list <- split(roads[, c("x", "y")], roads$street)

road.segments <- lapply(unique(roads$street), function(i) {
  dat <- roads[roads$street == i, ]
  names(dat)[names(dat) %in% c("x", "y")] <- c("x1", "y1")
  seg.data <- dat[-1, c("x1", "y1")]
  names(seg.data) <- c("x2", "y2")
  dat <- cbind(dat[-nrow(dat), ], seg.data)
  dat$id <- paste0(dat$street, "-", seq_len(nrow(dat)))
  dat
})

road.segments <- do.call(rbind, road.segments)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

roads.list <- split(roads[, c("x", "y")], roads$street)

plot(fatalities[, c("x", "y")], xlim = range(roads$x), ylim = range(roads$y),
     pch = 15, cex = 0.5, col = "gray", asp = 1)
invisible(lapply(roads.list, lines, col = "gray"))
points(HistData::Snow.pumps[, c("x", "y")], pch = 17, cex = 1, col = "blue")


## ------------------------------------------------------------------------
## The 18 cases at 38 Broad Street ##

broad38 <- c(239, 12, 310, 398, 562, 397, 421, 190, 290, 61, 174, 547, 523, 521,
             138, 59, 340, 508)

# With fatalities, all members of the stack have different coordinates

fatalities[fatalities$case %in% broad38, ]

# With fatalities.unstacked, all members of the stack have identical coordinates

fatalities.unstacked[fatalities.unstacked$case %in% broad38, ]


## ------------------------------------------------------------------------
# The 18 cases at 38 Broad street are nominally represented by case 239

fatalities.address[136:140, ]

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

## Graph parameters ##
bw <- 1:4
facets <- paste("Bandwidth =", bw)
x.range <- c(11.5, 13.5)
y.range <- c(10.5, 12.5)

## Data ##

Snow.deathsB <- lapply(rep("fatalities", max(bw)), get)
Snow.pumpsB <- lapply(rep("Snow.pumps", max(bw)), get)

for (i in seq_along(Snow.deathsB)) {
  Snow.deathsB[[i]]$facet <- facets[i]
}

Snow.deathsB2 <- Snow.deathsB
Snow.deathsB <- do.call(rbind, Snow.deathsB)

# Cambridge Street #
street.name <- "Cambridge Street"
cambridge.data <- roads[roads$name == street.name, ]
cambridge.data <- cambridge.data[order(cambridge.data$x), ]
d1 <- cambridge.data[-nrow(cambridge.data), c("x", "y")]
d2 <- cambridge.data[-1, c("x", "y")]

intercept.slope <-lapply(seq_len(nrow(cambridge.data) - 1), function(i) {
  coef(lm(y ~ x, data = rbind(d1[i, ], d2[i, ])))
})

sel <- 3
cambridge.angle <- atan(intercept.slope[[sel]][2]) * 180L / pi
cambridge.x <- mean(cambridge.data[sel:(sel + 1), "x"])
cambridge.y <- intercept.slope[[sel]][1] +
  intercept.slope[[sel]][2] * cambridge.x
cambridge.df <- data.frame(x = cambridge.x, y = cambridge.y)

# Broad Street #
street.name <- "Broad Street"
broad.data <- roads[roads$name == street.name, ]
broad.list <- roads.list[paste(unique(broad.data$street))]
broad.list <- lapply(broad.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

broad.pts.data <- do.call(rbind, broad.list)
broad.pts.data <- broad.pts.data[seq_len(nrow(broad.pts.data)) %% 2 != 0, ]

segment.ols <- lapply(broad.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "193"
seg.id <- do.call(rbind, strsplit(rownames(broad.pts.data), "[.]"))[, 1]
i <- which(seg.id == sel)

broad.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
broad.x <- median(broad.pts.data[i:(i + 1), "x"])
broad.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * broad.x
broad.df <- data.frame(x = broad.x, y = broad.y)

## Graph ##

p <- ggplot(data = Snow.deathsB, aes(x = x, y = y)) +
  geom_point(color = "gray") +
  geom_point(data = Snow.pumps, aes(x = x, y = y), color = "blue", pch = 2,
             size = 2.5, stroke = 0.75) +
  coord_fixed(xlim = x.range, ylim = y.range) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ facet, nrow = 2) +
  ggtitle("'Stacked' Fatalities")

for (i in seq_along(roads.list)) {
  p <- p + geom_path(data = roads.list[[i]], aes(x = x, y = y),
    color = "lightgray")
}

for (i in seq_along(bw)) {
  p <- p + geom_density_2d(data = Snow.deathsB2[[i]], aes(x = x, y = y),
    color = "red", size = 1/3, h = bw[i])
}

p + geom_text(data = broad.df, aes(x = x, y = y), label = "Broad St",
              angle = broad.angle) +
  geom_text(data = cambridge.df, aes(x = x, y = y), label = "Cambridge St",
            angle = cambridge.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

## Data ##

fatalities.addressB <- lapply(rep("fatalities.address", max(bw)), get)
fatalities.unstackedB <- lapply(rep("fatalities.unstacked", max(bw)), get)

for (i in seq_along(fatalities.addressB)) {
  fatalities.addressB[[i]]$facet <- facets[i]
}

for (i in seq_along(fatalities.unstackedB)) {
  fatalities.unstackedB[[i]]$facet <- facets[i]
}

fatalities.addressB <- do.call(rbind, fatalities.addressB)

## Graph ##

p <- ggplot(data = fatalities.addressB, aes(x = x, y = y)) +
  geom_point(color = "gray") +
  geom_point(data = Snow.pumps, aes(x = x, y = y), color = "blue", pch = 2,
             size = 2.5, stroke = 0.75) +
  coord_fixed(xlim = x.range, ylim = y.range) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ facet, nrow = 2) +
  ggtitle("'Unstacked' Fatalities")

for (i in seq_along(roads.list)) {
  p <- p + geom_path(data = roads.list[[i]], aes(x = x, y = y),
    color = "lightgray")
}

for (i in seq_along(bw)) {
  p <- p + geom_density_2d(data = fatalities.unstackedB[[i]], aes(x = x, y = y),
    color = "red", size = 1/3, h = bw[i])
}

p + geom_text(data = broad.df, aes(x = x, y = y), label = "Broad St",
              angle = broad.angle) +
  geom_text(data = cambridge.df, aes(x = x, y = y), label = "Cambridge St",
            angle = cambridge.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

plot(fatalities.address[, c("x", "y")], pch = 20, asp = 1, xlim = x.range,
  ylim = y.range, cex = 1/3)
invisible(lapply(roads.list, lines, col = "gray"))
points(Snow.pumps[, c("x", "y")], col = "blue", pch = 2, cex = 1.25, lwd = 1.5)
points(fatalities.address[, c("x", "y")], cex = fatalities.address$case.count,
  pch = 0)
kde2d <- KernSmooth::bkde2D(fatalities.unstacked[, c("x", "y")],
  bandwidth = rep(bandwidth, 2))
contour(x = kde2d$x1, y = kde2d$x2, z = kde2d$fhat, col = "gray", add = TRUE)
title(main = "'Unstacked' fatalities with proportionately-sized symbols")

# Cambridge Street #
street.name <- "Cambridge Street"
cambridge.data <- roads[roads$name == street.name, ]
cambridge.data <- cambridge.data[order(cambridge.data$x), ]
d1 <- cambridge.data[-nrow(cambridge.data), c("x", "y")]
d2 <- cambridge.data[-1, c("x", "y")]

intercept.slope <-lapply(seq_len(nrow(cambridge.data) - 1), function(i) {
  coef(lm(y ~ x, data = rbind(d1[i, ], d2[i, ])))
})

sel <- 3
cambridge.angle <- atan(intercept.slope[[sel]][2]) * 180L / pi
cambridge.x <- mean(cambridge.data[sel:(sel + 1), "x"])
cambridge.y <- intercept.slope[[sel]][1] +
               intercept.slope[[sel]][2] * cambridge.x
cambridge.df <- data.frame(x = cambridge.x, y = cambridge.y)

text(cambridge.df$x, cambridge.df$y, "Cambridge St", srt = cambridge.angle)

# Broad Street #
street.name <- "Broad Street"
broad.data <- roads[roads$name == street.name, ]
broad.list <- roads.list[paste(unique(broad.data$street))]
broad.list <- lapply(broad.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

broad.pts.data <- do.call(rbind, broad.list)
broad.pts.data <- broad.pts.data[seq_len(nrow(broad.pts.data)) %% 2 != 0, ]

segment.ols <- lapply(broad.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "193"
seg.id <- do.call(rbind, strsplit(rownames(broad.pts.data), "[.]"))[, 1]
i <- which(seg.id == sel)

broad.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
broad.x <- median(broad.pts.data[i:(i + 1), "x"])
broad.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * broad.x
broad.df <- data.frame(x = broad.x, y = broad.y)

text(broad.df$x, broad.df$y, "Broad St", srt = broad.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

case.select <- 12
case <- Snow.deaths[case.select, c("x", "y")]

plot(fatalities[, c("x", "y")], xlim = x.range, ylim = y.range, pch = NA,
  asp = 1)
invisible(lapply(roads.list, lines, col = "lightgray"))
points(fatalities[, c("x", "y")], col = "gray")
text(fatalities[case.select, c("x", "y")], labels = case.select, pos = 1,
  col = "red")
title(main = paste("Orthogonal Classification: Broad Street Case", case.select))

# "Unit" Circle
radius <- 0.5
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- fatalities[case.select, "x"] + radius *
                   cos(0:unit.base * unit.radians)
circumference.y <- fatalities[case.select, "y"] + radius *
                   sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y)

candidate <- c("216-1", "217-1", "243-1")
street.color <- c("red", "dodgerblue", "green2")

invisible(lapply(seq_along(candidate), function(i) {
  seg.data <- road.segments[road.segments$id == candidate[i],
    c("x1", "y1", "x2", "y2")]
  segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2,
    col = street.color[i], lwd = 2)
  points(seg.data[, c("x1", "y1")], pch = 0)
  points(seg.data[, c("x2", "y2")], pch = 0)
}))

invisible(lapply(seq_along(candidate), function(i) {
  seg.data <- road.segments[road.segments$id == candidate[i],
    c("x1", "y1", "x2", "y2")]
  seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                       y = c(seg.data$y1, seg.data$y2))
  ols <- lm(y ~ x, data = seg.df)
  segment.slope <- coef(ols)[2]
  segment.intercept <- coef(ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- case$y - orthogonal.slope * case$x
  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)
  y.proj <- segment.slope * x.proj + segment.intercept
  arrows(case$x, case$y, x.proj, y.proj, col = street.color[i], lwd = 2,
    length = 1/8)
  abline(ols, lty = "dashed", col = street.color[i], lwd = 1)
}))

points(fatalities[case.select, c("x", "y")], pch = 20, col = "red", cex = 1.5)
text(cambridge.df$x, cambridge.df$y, "Cambridge St", srt = cambridge.angle)
text(broad.df$x, broad.df$y, "Broad St", srt = broad.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

case.select <- 290
case <- Snow.deaths[case.select, c("x", "y")]

plot(fatalities[, c("x", "y")], xlim = x.range, ylim = y.range, pch = NA,
  asp = 1)
invisible(lapply(roads.list, lines, col = "lightgray"))
points(fatalities[, c("x", "y")], col = "gray")
text(fatalities[case.select, c("x", "y")], labels = case.select, pos = 1,
  col = "red")
title(main = paste("Classification Error: Broad Street Case", case.select))

# "Unit" Circle #
radius <- 0.5
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[case.select, "x"] + radius *
                   cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[case.select, "y"] + radius *
                   sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y)

candidate <- c("216-1", "243-2")
street.color <- c("black", "red")

invisible(lapply(seq_along(candidate), function(i) {
  seg.data <- road.segments[road.segments$id == candidate[i],
    c("x1", "y1", "x2", "y2")]
  segments(seg.data$x1, seg.data$y1, seg.data$x2, seg.data$y2,
    col = street.color[i], lwd = 2)
  points(seg.data[, c("x1", "y1")], pch = 0)
  points(seg.data[, c("x2", "y2")], pch = 0)
}))

invisible(lapply(seq_along(candidate), function(i) {
  seg.data <- road.segments[road.segments$id == candidate[i],
    c("x1", "y1", "x2", "y2")]
  seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                       y = c(seg.data$y1, seg.data$y2))
  ols <- lm(y ~ x, data = seg.df)
  segment.slope <- coef(ols)[2]
  segment.intercept <- coef(ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- case$y - orthogonal.slope * case$x
  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)
  y.proj <- segment.slope * x.proj + segment.intercept
  arrows(case$x, case$y, x.proj, y.proj, col = street.color[i], lwd = 2,
    length = 1/8)
  abline(ols, lty = "dashed", col = street.color[i], lwd = 1)
}))

text(cambridge.df$x, cambridge.df$y, "Cambridge St", srt = cambridge.angle)
text(broad.df$x, broad.df$y, "Broad St", srt = broad.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

broad.st.cases <- c(12, 21, 25, 32, 42, 72, 91, 93, 122, 190, 194, 212, 232,
                    239, 310, 321, 343, 373, 389, 396, 397, 398, 421, 496, 562,
                    570, 572, 574, 577)

orthogonal.projection <- lapply(broad.st.cases, function(i) {
  case <- fatalities[i, c("x", "y")]

  within.radius <- "216-1"

  ortho.proj.test <- lapply(within.radius, function(x) {
    seg.data <- road.segments[road.segments$id == x, c("x1", "y1", "x2", "y2")]

    seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                         y = c(seg.data$y1, seg.data$y2))

    ols <- lm(y ~ x, data = seg.df)
    segment.slope <- coef(ols)[2]
    segment.intercept <- coef(ols)[1]
    orthogonal.slope <- -1 / segment.slope
    orthogonal.intercept <- case$y - orthogonal.slope * case$x

    x.proj <- (orthogonal.intercept - segment.intercept) /
              (segment.slope - orthogonal.slope)

    y.proj <- segment.slope * x.proj + segment.intercept

    # segment bisection/intersection test

    distB <- dist(rbind(seg.df[1, ], c(x.proj, y.proj))) +
             dist(rbind(seg.df[2, ], c(x.proj, y.proj)))

    bisect.test <- signif(dist(seg.df)) == signif(distB)

    if (bisect.test) {
      ortho.dist <- c(dist(rbind(c(case$x, case$y), c(x.proj, y.proj))))
      ortho.pts <- data.frame(x.proj, y.proj)
      data.frame(id = x, ortho.pts, ortho.dist, stringsAsFactors = FALSE)
    } else NA
  })

  out <- do.call(rbind, ortho.proj.test)
  sel <- which.min(out$ortho.dist)
  out[sel, ]
})

ortho.proj <- do.call(rbind, orthogonal.projection)
rownames(ortho.proj) <- NULL
ortho.proj$case <- broad.st.cases
vars <- c("id", "case", "x.proj", "y.proj", "ortho.dist")
ortho.proj <- ortho.proj[, vars]

# ## Classification errors ##

road.segment.fix <- list("216-1" = c(290, 61, 174, 547, 523, 521, 138, 59, 340,
  508))

# Recompute orthogonal distances

ortho.projB <- lapply(seq_along(road.segment.fix), function(i) {
  case <- fatalities[unlist(road.segment.fix[[i]]), ]
  seg.id <- names(road.segment.fix[i])
  seg.data <- road.segments[road.segments$id == seg.id, ]

  seg.df <- data.frame(x = c(seg.data$x1, seg.data$x2),
                       y = c(seg.data$y1, seg.data$y2))

  ols <- lm(y ~ x, data = seg.df)
  segment.slope <- coef(ols)[2]
  segment.intercept <- coef(ols)[1]
  orthogonal.slope <- -1 / segment.slope
  orthogonal.intercept <- case$y - orthogonal.slope * case$x

  x.proj <- (orthogonal.intercept - segment.intercept) /
            (segment.slope - orthogonal.slope)

  y.proj <- segment.slope * x.proj + segment.intercept

  proj.data <- lapply(1:nrow(case), function(j) {
    dat <- rbind(case[j, c("x", "y")], c(x.proj[j], y.proj[j]))
    cbind(x.proj[j], y.proj[j], c(dist(dat)))
  })

  out <- data.frame(seg.id, case$case, do.call(rbind, proj.data),
    stringsAsFactors = FALSE)
  names(out) <- c("id", "case", "x.proj", "y.proj", "ortho.dist")
  out
})

ortho.projB <- do.call(rbind, ortho.projB)
rownames(ortho.projB) <- NULL

ortho.proj <- rbind(ortho.proj, ortho.projB)
ortho.proj <- ortho.proj[order(ortho.proj$case), ]

# Plot #

i <- "216-1"

case <- ortho.proj[ortho.proj$id == i, "case"]
ortho <- ortho.proj[ortho.proj$id == i, c("x.proj", "y.proj")]

vars <- c("x1", "y1", "x2", "y2")
seg.coord <- road.segments[road.segments$id == i, vars]

plot(fatalities[case, c("x", "y")], asp = 1, pch = NA)
invisible(lapply(roads.list, lines, col = "lightgray"))
segments(seg.coord$x1, seg.coord$y1, seg.coord$x2, seg.coord$y2, col = "red")

points(seg.coord[, c("x1", "y1")], pch = 0)
points(seg.coord[, c("x2", "y2")], pch = 0)

proj.dat <- cbind(ortho, fatalities[case, c("x", "y")])

invisible(apply(proj.dat, 1, function(z) {
  segments(z["x"], z["y"], z["x.proj"], z["y.proj"], col = "lightgray")
}))

text(fatalities[case, c("x", "y")], labels = case, cex = 0.9)
title(main = "Virtues of Orthogonal Projection: Broad Street")

# Cambridge Street #
street.name <- "Cambridge Street"
cambridge.data <- roads[roads$name == street.name, ]
cambridge.data <- cambridge.data[order(cambridge.data$x), ]
d1 <- cambridge.data[-nrow(cambridge.data), c("x", "y")]
d2 <- cambridge.data[-1, c("x", "y")]

intercept.slope <- lapply(seq_len(nrow(cambridge.data) - 1), function(i) {
  coef(lm(y ~ x, data = rbind(d1[i, ], d2[i, ])))
})

sel <- 2
cambridge.angle <- atan(intercept.slope[[sel]][2]) * 180L / pi
cambridge.x <- mean(cambridge.data[sel:(sel + 1), "x"])
cambridge.y <- intercept.slope[[sel]][1] +
  intercept.slope[[sel]][2] * cambridge.x
cambridge.df <- data.frame(x = cambridge.x, y = cambridge.y)

text(cambridge.df$x, cambridge.df$y, "Cambridge St", srt = cambridge.angle)

# Broad Street #
street.name <- "Broad Street"
broad.data <- roads[roads$name == street.name, ]
broad.list <- roads.list[paste(unique(broad.data$street))]
broad.list <- lapply(broad.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

broad.pts.data <- do.call(rbind, broad.list)
broad.pts.data <- broad.pts.data[seq_len(nrow(broad.pts.data)) %% 2 != 0, ]

segment.ols <- lapply(broad.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "203"
seg.id <- do.call(rbind, strsplit(rownames(broad.pts.data), "[.]"))[, 1]
i <- which(seg.id == sel)

broad.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
broad.x <- median(broad.pts.data[i:(i + 1), "x"])
broad.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * broad.x
broad.df <- data.frame(x = broad.x, y = broad.y)

text(broad.df$x, broad.df$y, "Broad St", srt = broad.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

sel <- c(321, 239)

sig.fig <- 2

plot(fatalities[sel, c("x", "y")], asp = 1, pch = NA)
invisible(lapply(roads.list, lines, col = "lightgray"))
segments(seg.coord$x1, seg.coord$y1, seg.coord$x2, seg.coord$y2, col = "gray")
points(fatalities[fatalities$case %in% sel, c("x", "y")],
  col = c("red", "blue"))

obs <- fatalities[sel, c("x", "y")]
ort <- ortho.proj[ortho.proj$case %in% sel, c("x.proj", "y.proj")]
arrows(obs[1, "x"], obs[1, "y"], ort[1, "x.proj"], ort[1, "y.proj"],
  col = "blue", length = 1/8)
arrows(obs[2, "x"], obs[2, "y"], ort[2, "x.proj"], ort[2, "y.proj"],
  col = "red", length = 1/8)
points(ort[1, "x.proj"], ort[1, "y.proj"], pch = 0, col = "blue")
points(ort[2, "x.proj"], ort[2, "y.proj"], pch = 0, col = "red")

text(fatalities[fatalities$case %in% sel, c("x", "y")],
  labels = fatalities[fatalities$case %in% sel, "case"], pos = 2,
  col = c("red", "blue"))

lab.1 <- paste0("(", round(obs[1, "x"], sig.fig), ", ", round(obs[1, "y"],
  sig.fig), ")")
lab.2 <- paste0("(", round(obs[2, "x"], sig.fig), ", ", round(obs[2, "y"],
  sig.fig), ")")
lab.3 <- paste0("(", round(ort[1, "x.proj"], sig.fig), ", ",
  round(ort[1, "y.proj"], sig.fig), ")")
lab.4 <- paste0("(", round(ort[2, "x.proj"], sig.fig), ", ",
  round(ort[2, "y.proj"], sig.fig), ")")

text(fatalities[fatalities$case == 321, c("x", "y")], labels = lab.1,
  pos = 4, cex = 0.8, col = "blue")
text(fatalities[fatalities$case == 239, c("x", "y")], labels = lab.2,
  pos = 4, cex = 0.8, col = "red")
text(ort[1, ], labels = lab.3, pos = 2, cex = 0.8, col = "blue")
text(ort[2, ], labels = lab.4, pos = 4, cex = 0.8, col = "red")

title(main = "North v. South: Broad Street Cases 321 & 239")

# Broad Street #
street.name <- "Broad Street"
broad.data <- roads[roads$name == street.name, ]
broad.list <- roads.list[paste(unique(broad.data$street))]
broad.list <- lapply(broad.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

broad.pts.data <- do.call(rbind, broad.list)
broad.pts.data <- broad.pts.data[seq_len(nrow(broad.pts.data)) %% 2 != 0, ]

segment.ols <- lapply(broad.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "216"
seg.id <- do.call(rbind, strsplit(rownames(broad.pts.data), "[.]"))[, 1]
i <- which(seg.id == sel)

broad.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
broad.x <- median(broad.pts.data[i:(i + 1), "x"])
broad.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * broad.x
broad.df <- data.frame(x = broad.x, y = broad.y)

text(broad.df$x, broad.df$y, "Broad St", srt = broad.angle)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

cutpoint <- 0.05

i <- "216-1"

case <- ortho.proj[ortho.proj$id == i, "case"]
ortho <- ortho.proj[ortho.proj$id == i, c("x.proj", "y.proj")]

orientation <- sign(Snow.deaths[case, c("x", "y")] - ortho)

if (all(orientation$x * orientation$y == -1)) {
  orientation$side <- ifelse(orientation$x == 1 & orientation$y == -1, 0, 1)
}

if (all(orientation$x * orientation$y == 1)) {
  orientation$side <- ifelse(orientation$x == 1 & orientation$y == 1, 0, 1)
}

if (length(unique(orientation$side)) == 2) {
  sideA <- as.numeric(rownames(orientation[orientation$side == 0, ]))
  sideB <- as.numeric(rownames(orientation[orientation$side == 1, ]))
  data.South <- ortho.proj[ortho.proj$case %in% sideA, c("x.proj", "y.proj")]
  data.North <- ortho.proj[ortho.proj$case %in% sideB, c("x.proj", "y.proj")]

  rownames(data.South) <- sideA
  rownames(data.North) <- sideB

  if (nrow(data.South) >= 2) {
    clusterA <- hclust(dist(data.South))
    outA <- cutree(clusterA, h = cutpoint)
  } else {
    outA <- 1
    names(outA) <- rownames(data.South)
  }

  if (nrow(data.North) >= 2) {
    clusterB <- hclust(dist(data.North))
    outB <- cutree(clusterB, h = cutpoint)
  } else {
    outB <- 1
    names(outB) <- rownames(data.North)
  }

  outB <- max(outA) +  outB
  census <- c(outA, outB)
  out <- data.frame(case = as.numeric(names(census)), group = census,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
}

plot(clusterB, main = "Broad Street: North Side")
abline(h = 0.05, lty = "dashed", col = "blue")
axis(4, at = 0.05, labels = 0.05, col.axis = "blue", col.ticks = "blue")

plot(clusterA, main = "Broad Street: South Side")
abline(h = 0.05, lty = "dashed", col = "red")
axis(4, at = 0.05, labels = 0.05, col.axis = "red", col.ticks = "red")


