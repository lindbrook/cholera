## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cholera)
library(HistData)

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

street.list <- split(Snow.streets[, c("x", "y")], Snow.streets$street)

plot(Snow.deaths$x, Snow.deaths$y, pch = 20, cex = 0.5,
  xlim = range(Snow.streets$x), ylim = range(Snow.streets$y),
  xlab = "x", ylab = "y", asp = 1)
invisible(lapply(street.list, lines, lwd = 0.75))
points(Snow.pumps$x, Snow.pumps$y, pch = 17, col = "blue")

## ------------------------------------------------------------------------
duplicates <- Snow.deaths[(duplicated(Snow.deaths[, c("x", "y")])), ]

duplicates.id <- lapply(duplicates$x, function(i) {
  Snow.deaths[Snow.deaths$x == i, "case"]
})

Snow.deaths[unlist(duplicates.id), ]

## ------------------------------------------------------------------------
fatalities <- Snow.deaths

fix <- data.frame(x = c(12.56974, 12.53617, 12.33145), y = c(11.51226, 11.58107, 14.80316))

fatalities[c(91, 93, 209), c("x", "y")] <- fix

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

# Cambridge Street #
street.name <- "Cambridge Street"
cam.data <- roads[roads$name == street.name, ]
cam.data <- cam.data[order(cam.data$x), ]
d1 <- cam.data[-nrow(cam.data), c("x", "y")]
d2 <- cam.data[-1, c("x", "y")]

intercept.slope <- lapply(seq_len(nrow(cam.data) - 1), function(i) {
  coef(lm(y ~ x, data = rbind(d1[i, ], d2[i, ])))
})

sel <- 2
cam.angle <- atan(intercept.slope[[sel]][2]) * 180L / pi
cam.x <- mean(cam.data[2:3, "x"])
cam.y <- intercept.slope[[sel]][1] + intercept.slope[[sel]][2] * cam.x

# Broad Street #
street.name <- "Broad Street"
broad.data <- roads[roads$name == street.name, ]
broad.list <- street.list[paste(unique(broad.data$street))]
broad.list <- lapply(broad.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

broad.pts.data <- do.call(rbind, broad.list)
broad.pts.data <- broad.pts.data[seq_len(nrow(broad.pts.data)) %% 2 != 0, ]

segment.ols <- lapply(broad.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "216"
broad.angle <- atan(segment.ols$`216`["x"]) * 180 / pi
broad.x <- median(broad.pts.data[5:6, "x"])
broad.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * broad.x

# Plot #
plot(Snow.deaths[, c("x", "y")], xlim = c(12, 13), ylim = c(10.9, 11.9),
     pch = NA, cex = 0.5, col = "blue", asp = 1)
invisible(lapply(street.list, lines, col = "lightgray"))
points(Snow.pumps[, c("x", "y")], col = "blue", pch = 2, cex = 1)
text(Snow.pumps[, c("x", "y")], col = "blue", labels = "Broad Street Well",
     pos = 4, cex = 0.85)
points(Snow.deaths[-unlist(duplicates.id), c("x", "y")])
title(main = "The Three Pairs of Duplicate Cases")

invisible(lapply(duplicates.id, function(x) {
  points(Snow.deaths[c(x[1], x[2]), c("x", "y")], pch = 1:0, cex = c(1, 1.75),
    col = c("black", "red"))
}))

id.data <- do.call(rbind, duplicates.id)
top.data <- Snow.deaths[id.data[, 1], c("x", "y")]
bottom.data <- Snow.deaths[id.data[, 2], c("x", "y")]
text(top.data, labels = rownames(top.data), pos = c(3, 2), cex = 0.9,
     col = "red")
text(bottom.data[1:2, ], labels = rownames(bottom.data)[1:2], pos = 1,
     cex = 0.9)
text(bottom.data[3, c("x", "y")] + c(0.03, -0.03),
     labels = rownames(bottom.data)[3], cex = 0.9)
legend(x = "bottomleft",
       legend = c("Well", "Duplicate", "Case"),
       col = c("blue", "red", "black"),
       pch = c(2, 0, 1),
       bg = "white",
       cex = 0.8,
       title = "Key")

text(cam.x, cam.y, "Cambridge St", srt = cam.angle)
text(broad.x, broad.y, "Broad St", srt = broad.angle)

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

broad.40 <- c(32, 122)

plot(Snow.deaths[, c("x", "y")], xlim = c(12, 13), ylim = c(10.9, 11.9),
     pch = NA, cex = 0.5, col = "blue", asp = 1)
invisible(lapply(street.list, lines, col = "lightgray"))

points(Snow.deaths[-c(unlist(duplicates.id), broad.40), c("x", "y")])

points(Snow.pumps[, c("x", "y")], col = "blue", pch = 2, cex = 1)
text(Snow.pumps[, c("x", "y")], col = "blue", labels = "Broad Street Well",
     pos = 4, cex = 0.85)

points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")
text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     cex = 0.9, pos = 4, col = "#009E73")

title(main = "40 Broad Street:\nTwo (32 & 122) Rather than Four Cases")

invisible(lapply(duplicates.id, function(x) {
  points(Snow.deaths[x[1], c("x", "y")])
}))

legend(x = "bottomleft",
       legend = c("Well", "Case", "40 Broad St"),
       col = c("blue", "black", "#009E73"),
       pch = c(2, 1, 1),
       bg = "white",
       cex = 0.8,
       title = NULL)

text(cam.x, cam.y, "Cambridge St", srt = cam.angle)
text(broad.x, broad.y, "Broad St", srt = broad.angle)

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

# Noel Street #
street.name <- "Noel Street"
noel.data <- roads[roads$name == street.name, ]
noel.list <- street.list[paste(unique(noel.data$street))]
noel.list <- lapply(noel.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

segment.ols <- lapply(noel.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "118"

noel.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
noel.x <- abs(noel.list[[sel]][1, "x"] - noel.list[[sel]][2, "x"]) / 3 +
  noel.list[[sel]][2, "x"]
noel.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * noel.x

# Poland Street #
street.name <- "Poland Street"
poland.data <- roads[roads$name == street.name, ]
poland.list <- street.list[paste(unique(poland.data$street))]
poland.list <- lapply(poland.list, function(df) {
  df[order(df$x, decreasing = TRUE), ]
})

segment.ols <- lapply(poland.list, function(x) {
  coef(lm(y ~ x, data = x))
})

sel <- "140"

poland.angle <- atan(segment.ols[[sel]]["x"]) * 180 / pi
poland.x <- abs(poland.list[[sel]][1, "x"] - poland.list[[sel]][2, "x"]) /
            4 + poland.list[[sel]][2, "x"]
poland.y <- segment.ols[[sel]][1] + segment.ols[[sel]][2] * poland.x

# Plot #
noel.street <- c(282, 422)

plot(Snow.deaths[, c("x", "y")], xlim = c(11.75, 12.75), ylim = c(14.25, 15.25),
     asp = 1, pch = NA)
invisible(lapply(street.list, lines, col = "lightgray"))

points(Snow.deaths[-noel.street, c("x", "y")])

points(Snow.deaths[noel.street, c("x", "y")], col = "#009E73")
text(Snow.deaths[noel.street, c("x", "y")], cex = 0.9, pos = 4,
     labels = Snow.deaths$case[noel.street], col = "#009E73")

title(main = paste0("15 Noel Street:", "\n",
                    " Two (282 & 422) Rather than Three Cases"))

legend(x = "bottomleft",
       legend = c("Case", "15 Noel St"),
       col = c("black", "#009E73"),
       pch = 1,
       bg = "white",
       cex = 0.8,
       title = NULL)

text(noel.x, noel.y, "Noel St", srt = noel.angle)
text(poland.x, poland.y, "Poland St", srt = poland.angle)

## ---- echo = FALSE-------------------------------------------------------

# Equation of line for the "home" road segment
segmentOLS <- function(segment) {
  lm(y ~ x, data = street.list[[segment]])
}

# Slope for orthogonal stack axis
orthogonalSlope <- function(segment) {
  ols <- segmentOLS(segment)
  -1 / coef(ols)[2]
}

# Intercept for orthogonal stack axis
orthogonalIntercept <- function(case, segment = sel) {
  Snow.deaths[case, "y"] - orthogonalSlope(segment) * Snow.deaths[case, "x"]
}

# Euclidean distance between observed points
unitDistance <- function(case1, case2) {
  dist(Snow.deaths[c(case1, case2), c("x", "y")])
}

# Quadratic equation
quadratic <- function(a, b, c) {
  root1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  root2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
  c(root1, root2)
}

# Geometric interpolation
interpolatedPoints <- function(case1, case2, multiplier = 0.5, segment = sel) {
  p <- Snow.deaths[case1, "x"]
  q <- Snow.deaths[case1, "y"]
  radius <- multiplier * unitDistance(case1, case2)
  m <- orthogonalSlope(segment)
  b <- orthogonalIntercept(case1)
  A <- (m^2 + 1)
  B <- 2 * (m * b - m * q - p)
  C <- (q^2 - radius^2 + p^2 - 2 * b * q + b^2)
  quadratic(A, B, C)
}


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

sel <- "216"  # street list element, one with 2 endpoints

plot(Snow.deaths[, c("x", "y")], xlim = c(12, 13), ylim = c(10.9, 11.9),
     pch = NA, cex = 0.5, col = "blue", asp = 1)

invisible(lapply(street.list[names(street.list) != sel], lines,
                 col = "lightgray"))

points(Snow.pumps[, c("x", "y")], col = "blue", pch = 2, cex = 1)

text(Snow.pumps[, c("x", "y")], col = "blue", labels = "Broad Street Well",
     pos = 4, cex = 0.85)

points(Snow.deaths[-c(broad.40, duplicates$case), c("x", "y")], col = "gray")
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")

text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     cex = 0.9, pos = 4, col = "#009E73")

seg.data <- unlist(street.list[[sel]])

segments(seg.data["x1"], seg.data["y1"], seg.data["x2"], seg.data["y2"],
         col = "#009E73")

title(main = paste0("Geometric Interpolation: Candidate Locations for the",
                    "\n", " Two Missing Cases at 40 Broad Street"))

ols <- segmentOLS(sel)
segment.slope <- coef(ols)[2]
segment.intercept <- coef(ols)[1]
orthogonal.slope <- orthogonalSlope(sel)

orthogonal.intercept.32 <- orthogonalIntercept(32)
orthogonal.intercept.122 <- orthogonalIntercept(122)

move.candidates <- c(91, 93, 209)
stay.in.place <- c(214, 241, 429)
candidates <- c(move.candidates, stay.in.place)

x.out1 <- max(interpolatedPoints(32, 122))
x.out2 <- max(interpolatedPoints(32, 122, multiplier = 1.5))
y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
y.out2 <- orthogonal.slope * x.out2 + orthogonal.intercept.32

points(x.out1, y.out1, col = "red", pch = 4)
points(x.out2, y.out2, col = "red", pch = 4)
text(x.out1, y.out1, col = "red", labels = "one missing case", pos = 4,
     cex = 0.8)
text(x.out2, y.out2, col = "red", labels = "other missing case", pos = 4,
     cex = 0.8)

text(cam.x, cam.y, "Cambridge St", srt = cam.angle)
text(broad.x, broad.y, "Broad St", srt = broad.angle)

legend(x = "bottomleft",
       legend = c("Well", "Broad St", "Candidate", "Others"),
       col = c("blue", "#009E73", "red", "gray"),
       pch = c(2, 1, 4, 1),
       bg = "white",
       title = "Key")


## ---- fig.width = 5, fig.height = 3.5, echo = FALSE----------------------

plot(0:3, rep(0, 4), xlim = c(0, 3.6), ylim = c(0, 1), pch = NA, xaxt = "n",
     yaxt = "n", bty = "n", ann = FALSE)
axis(1, seq(0.3, 3.3, 1), labels = 0:3)
points(c(0.3, 2.3), rep(0.5, 2), col = "#009E73")
text(c(0.3, 2.3), rep(0.5, 2), labels = c(32, 122), pos = 1, cex = 0.9,
     col = "#009E73")
arrows(0.3, 0.6, 2.3, 0.6, code = 3, length = 0.067, angle = 90, col = "blue")
arrows(0.3, 0.6, 2.3, 0.6, code = 3, length = 0.1, col = "blue")
text(1.3, 0.7, "2 units", col = "blue")
title(main = "Broad Street Unit Distance")


## ---- fig.width = 5, fig.height = 3.5, echo = FALSE----------------------
plot(0:3, rep(0, 4), xlim = c(0, 3.6), ylim = c(0, 1), pch = NA, xaxt = "n",
     yaxt = "n", bty = "n", ann = FALSE)
axis(1, seq(0.3, 3.3, 1), labels = 0:3)
points(c(0.3, 2.3), rep(0.5, 2), col = "#009E73")
text(c(0.3, 2.3), rep(0.5, 2), labels = c(32, 122), pos = 1, cex = 0.9,
     col = "#009E73")
points(1.3, 0.5, col = "red", pch = 4)
text(1.3, 0.5, labels = "missing", pos = 1, col = "red")
arrows(0.3, 0.6, 1.3, 0.6, code = 3, length = 0.067, angle = 90, col = "blue")
arrows(0.3, 0.6, 1.3, 0.6, length = 0.1, col = "blue")
text(0.8, 0.7, "1 unit", col = "blue")
title(main = "Location for One Missing Case")

## ---- fig.width = 5, fig.height = 3.5, echo = FALSE----------------------
plot(0:3, rep(0, 4), xlim = c(0, 3.6), ylim = c(0, 1), pch = NA, xaxt = "n",
     yaxt = "n", bty = "n", ann = FALSE)
axis(1, seq(0.3, 3.3, 1), labels = 0:3)
points(c(0.3, 2.3), rep(0.5, 2), col = "#009E73")
text(c(0.3, 2.3), rep(0.5, 2), labels = c(32, 122), pos = 1, cex = 0.9,
     col = "#009E73")
points(1.3, 0.5, pch = 4)
points(3.3, 0.5, col = "red", pch = 4)
text(3.3, 0.5, labels = "missing", pos = 1, col = "red")
arrows(0.3, 0.6, 3.3, 0.6, code = 3, length = 0.067, angle = 90, col = "blue")
arrows(0.3, 0.6, 3.3, 0.6, length = 0.1, col = "blue")
text(1.8, 0.7, "3 units", col = "blue")
title(main = "Location for Other Missing Case")


## ---- echo = FALSE-------------------------------------------------------

sel <- "216"  # street list element, one with 2 endpoints

ols <- segmentOLS(sel)
segment.slope <- coef(ols)[2]
segment.intercept <- coef(ols)[1]
orthogonal.slope <- orthogonalSlope(sel)
orthogonal.intercept.32 <- orthogonalIntercept(32)

angle <- atan(orthogonal.slope) * 180 / pi

x.pt <- (orthogonal.intercept.32 - segment.intercept) /
  (segment.slope - orthogonal.slope)

y.pt <- segment.slope * x.pt + segment.intercept

x.out2 <- max(interpolatedPoints(32, 122, multiplier = 2))
y.out2 <- orthogonal.slope * x.out2 + orthogonal.intercept.32

meter0 <- function(x, y, radius = 0.025) {
  p <- x
  q <- y
  m <- segment.slope
  b <- q - p * segment.slope
  A <- (m^2 + 1)
  B <- 2 * (m * b - m * q - p)
  C <- (q^2 - radius^2 + p^2 - 2 * b * q + b^2)
  c(max(quadratic(A, B, C)), b)
}

meter <- function(case, radius = 0.025) {
  p <- Snow.deaths[case, "x"]
  q <- Snow.deaths[case, "y"]
  m <- segment.slope
  b <- q - p * segment.slope
  A <- (m^2 + 1)
  B <- 2 * (m * b - m * q - p)
  C <- (q^2 - radius^2 + p^2 - 2 * b * q + b^2)
  c(max(quadratic(A, B, C)), b)
}

meterB <- function(x, y, radius = 0.025) {
  p <- x
  q <- y
  m <- segment.slope
  b <- q - p * segment.slope
  A <- (m^2 + 1)
  B <- 2 * (m * b - m * q - p)
  C <- (q^2 - radius^2 + p^2 - 2 * b * q + b^2)
  c(quadratic(A, B, C), b)
}


## ---- fig.width = 5, fig.height = 5, echo = FALSE------------------------

center <- Snow.deaths[32, c("x", "y")]
delta <- 0.1

plot(Snow.deaths[, c("x", "y")], xlim = c(center$x - delta, center$x + delta),
     ylim = c(center$y - delta, center$y + delta),
     pch = NA, col = "blue", asp = 1)

abline(a = orthogonal.intercept.32, b = orthogonal.slope, lty = "dotted",
       lwd = 1.5)
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")
text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     pos = 2, col = "#009E73")

x.out1 <- max(interpolatedPoints(32, 122))
y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
points(x.out1, y.out1, pch = 4, col = "red", lwd = 2)
text(x.out1, y.out1, labels = "missing observation", pos = 2, col = "red")

x1 <- meter("32", 0.0125)[1]
y1 <- segment.slope * x1 + meter("32")[2]
x2 <- meter0(x.out1, y.out1, 0.0125)[1]
y2 <- segment.slope * x2 + meter0(x.out1, y.out1, 0.0125)[2]
arrows(x1, y1, x2, y2, code = 3, length = 0.05, angle = 90, col = "blue")
arrows(x1, y1, x2, y2, length = 0.1, col = "blue")

x1.label <- meter0(x1, y1, 0.0125)[1]
y1.label <- segment.slope * x1.label + meter0(x1, y1, 0.0125)[2]
x2.label <- meter0(x2, y2, 0.0125)[1]
y2.label <- segment.slope * x2.label + meter0(x2, y2, 0.0125)[2]
text(mean(c(x1.label, x2.label)), mean(c(y1.label, y2.label)), labels = "1 unit",
     srt = angle, col = "blue")

abline(ols)


## ---- fig.width = 5, fig.height = 5, echo = FALSE------------------------
plot(Snow.deaths[, c("x", "y")], xlim = c(center$x - delta, center$x + delta),
     ylim = c(center$y - delta, center$y + delta),
     pch = NA, col = "blue", asp = 1)

abline(a = orthogonal.intercept.32, b = orthogonal.slope, lty = "dotted",
       lwd = 1.5)
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")
text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     pos = 2, col = "#009E73")

broad.df <- Snow.deaths[broad.40, c("x", "y")]

r <- dist(broad.df) / 2  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[32, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[32, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

arrows(broad.df["32", "x"],
       broad.df["32", "y"],
       broad.df["32", "x"] + r,
       broad.df["32", "y"],
       col = "blue", length = 0.1)

text(broad.df["32", "x"] + 0.5 * r, broad.df["32", "y"], pos = 3,
     labels = "1 unit", col = "blue", cex = 0.9)

abline(ols)

## ---- fig.width = 5, fig.height = 5, echo = FALSE------------------------
plot(Snow.deaths[, c("x", "y")], xlim = c(center$x - delta, center$x + delta),
     ylim = c(center$y - delta, center$y + delta),
     pch = NA, col = "blue", asp = 1)

abline(a = orthogonal.intercept.32, b = orthogonal.slope, lty = "dotted", lwd = 1.5)
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")
text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     pos = 2, col = "#009E73")

x.out1 <- max(interpolatedPoints(32, 122))
y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
points(x.out1, y.out1, pch = 15, col = "red")
text(x.out1, y.out1, labels = "point 2", pos = 4, col = "red")

x.out1 <- min(interpolatedPoints(32, 122))
y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
points(x.out1, y.out1, pch = 15, col = "red")
text(x.out1, y.out1, labels = "point 1", pos = 2, col = "red")

broad.df <- Snow.deaths[broad.40, c("x", "y")]

r <- dist(broad.df) / 2  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[32, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[32, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

arrows(broad.df["32", "x"],
       broad.df["32", "y"],
       broad.df["32", "x"] + r,
       broad.df["32", "y"],
       col = "blue", length = 0.125)

text(broad.df["32", "x"] + 0.5 * r, broad.df["32", "y"], pos = 3,
     labels = "1 unit", col = "blue")

abline(ols)

## ---- eval = FALSE-------------------------------------------------------
#  # Equation of line for the "home" road segment
#  # "segment" is the numeric ID of one of the 528 sets of line segments in Dodson and Tobler
#  segmentOLS <- function(segment) {
#    lm(y ~ x, data = street.list[[segment]])
#  }
#  
#  # Slope for orthogonal stack axis
#  orthogonalSlope <- function(segment) {
#    ols <- segmentOLS(segment)
#    -1 / coef(ols)[2]
#  }
#  
#  # Intercept for orthogonal stack axis
#  # "case" if the reference point of a stack
#  orthogonalIntercept <- function(case, segment) {
#    Snow.deaths[case, "y"] - orthogonalSlope(segment) * Snow.deaths[case, "x"]
#  }
#  
#  # unit distance is a function of the Euclidean distance between "case1" and "case2"
#  unitDistance <- function(case1, case2) {
#    dist(Snow.deaths[c(case1, case2), c("x", "y")])
#  }
#  
#  # Quadratic equation
#  quadratic <- function(a, b, c) {
#    root1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
#    root2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
#    c(root1, root2)
#  }
#  
#  # Geometric interpolation
#  # "multiplier" scales and defines the unit distance
#  interpolatedPoints <- function(case1, case2, multiplier = 0.5, segment) {
#    p <- Snow.deaths[case1, "x"]
#    q <- Snow.deaths[case1, "y"]
#    radius <- multiplier * unitDistance(case1, case2)
#    m <- orthogonalSlope(segment)
#    b <- orthogonalIntercept(case1)
#    A <- (m^2 + 1)
#    B <- 2 * (m * b - m * q - p)
#    C <- (q^2 - radius^2 + p^2 - 2 * b * q + b^2)
#    quadratic(A, B, C)
#  }
#  

## ---- fig.width = 5, fig.height = 5, echo = FALSE------------------------

plot(Snow.deaths[, c("x", "y")], xlim = c(center$x - delta, center$x + delta),
     ylim = c(center$y - delta, center$y + delta),
     pch = NA, col = "blue", asp = 1)

abline(a = orthogonal.intercept.32, b = orthogonal.slope, lty = "dotted",
       lwd = 1.5)
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")
text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     pos = 2, col = "#009E73")

x.out1 <- max(interpolatedPoints(32, 122))
y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
points(x.out1, y.out1, pch = 4, col = "red", lwd = 2)
text(x.out1, y.out1, labels = "missing observation", pos = 4, col = "red",
     cex = 0.95)

broad.df <- Snow.deaths[broad.40, c("x", "y")]

r <- dist(broad.df) / 2  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[32, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[32, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

arrows(broad.df["32", "x"],
       broad.df["32", "y"],
       broad.df["32", "x"] + r,
       broad.df["32", "y"],
       col = "blue", length = 0.125)

text(broad.df["32", "x"] + 0.5 * r, broad.df["32", "y"], pos = 3,
     labels = "1 unit", col = "blue", cex = 0.9)

abline(ols)


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

sel <- "216"  # street list element, one with 2 endpoints

plot(Snow.deaths[, c("x", "y")], xlim = c(12, 13), ylim = c(10.9, 11.9),
     pch = NA, cex = 0.5, col = "blue", asp = 1)

invisible(lapply(street.list[names(street.list) != sel], lines,
                 col = "lightgray"))

points(Snow.pumps[, c("x", "y")], col = "blue", pch = 2, cex = 1)

text(Snow.pumps[, c("x", "y")], col = "blue", labels = "Broad Street Well",
     pos = 4, cex = 0.85)

points(Snow.deaths[-c(broad.40, unlist(duplicates.id)), c("x", "y")])
points(Snow.deaths[broad.40, c("x", "y")], col = "#009E73")

points(bottom.data)
points(top.data[3, ], col = "red", pch = 0, cex = 1.75)

text(Snow.deaths[broad.40, c("x", "y")], labels = Snow.deaths$case[broad.40],
     cex = 0.9, pos = 4)

text(bottom.data[1:2, ], labels = rownames(bottom.data)[1:2], pos = 1,
     cex = 0.9)
text(bottom.data[3, c("x", "y")] + c(0.03, -0.03), cex = 0.9,
     labels = rownames(bottom.data)[3])
text(top.data[3, ], labels = rownames(top.data)[3], pos = 3, cex = 0.9,
     col = "red")

seg.data <- unlist(street.list[[sel]])

segments(seg.data["x1"], seg.data["y1"], seg.data["x2"], seg.data["y2"],
         col = "#009E73")

title(main = paste0("Two Duplicate Cases (91 & 93) as Substitutes for ", "\n",
                    "the Two Missing Cases at 40 Broad Street"))

ols <- segmentOLS(sel)
segment.slope <- coef(ols)[2]
segment.intercept <- coef(ols)[1]
orthogonal.slope <- orthogonalSlope(sel)

orthogonal.intercept.32 <- orthogonalIntercept(32)
orthogonal.intercept.122 <- orthogonalIntercept(122)

move.candidates <- c(91, 93, 209)
stay.in.place <- c(214, 241, 429)
candidates <- c(move.candidates, stay.in.place)

x.out1 <- max(interpolatedPoints(32, 122))
x.out2 <- max(interpolatedPoints(32, 122, multiplier = 1.5))

y.out1 <- orthogonal.slope * x.out1 + orthogonal.intercept.32
y.out2 <- orthogonal.slope * x.out2 + orthogonal.intercept.32

broad40.32 <- Snow.deaths[32, c("x", "y")]
broad40.122 <- Snow.deaths[122, c("x", "y")]

broad.df <- data.frame(x = c(broad40.32$x, broad40.122$x),
                       y = c(broad40.32$y, broad40.122$y))

r <- dist(broad.df) / 2  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[32, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[32, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

r <- dist(broad.df) * 1.5  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[32, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[32, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

x.pt <- (orthogonal.intercept.32 - segment.intercept) /
        (segment.slope - orthogonal.slope)

y.pt <- segment.slope * x.pt + segment.intercept

segments(x.pt, y.pt, x.out2, y.out2, col = "#009E73")

points(x.out1, y.out1, pch = 0, col = "red")
points(x.out2, y.out2, pch = 0, col = "red")

ordered.pair1 <- paste0("(", round(x.out1[1], 3), ", ", round(y.out1[1], 3),
                        ")")

ordered.pair2 <- paste0("(", round(x.out2[1], 3), ", ", round(y.out2[1], 3),
                        ")")

arrows(Snow.deaths[93, "x"], Snow.deaths[93, "y"], x.out1, y.out1,
       col = "black", length = 1/8)

arrows(Snow.deaths[91, "x"], Snow.deaths[91, "y"], x.out2, y.out2,
       col = "black", length = 1/8)

text(x.out1, y.out1, pos = 2, col = "red", cex = 0.9,
     labels = paste(move.candidates[2], ordered.pair1))
text(x.out2, y.out2, pos = 1, col = "red", cex = 0.9,
     labels = paste(move.candidates[1], ordered.pair2))

legend(x = "bottomleft",
       legend = c("Well", "Duplicate", "Case", "40 Broad St"),
       col = c("blue", "red", "black", "#009E73"),
       pch = c(2, 0, 1, 1),
       bg = "white",
       title = "Key")

text(cam.x, cam.y, "Cambridge St", srt = cam.angle)
text(broad.x, broad.y, "Broad St", srt = broad.angle)

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------

noel.street <- c(282, 422)

sel <- "118" # street list element, one with 2 endpoints

plot(Snow.deaths[, c("x", "y")], xlim = c(11.75, 12.75), ylim = c(14.25, 15.25),
     asp = 1, pch = NA)

invisible(lapply(street.list[names(street.list) != sel], lines,
                 col = "lightgray"))

seg.data <- unlist(street.list[[sel]])
segments(seg.data["x1"], seg.data["y1"], seg.data["x2"], seg.data["y2"],
         col = "#009E73")

points(Snow.deaths[-noel.street, c("x", "y")])

points(Snow.deaths[noel.street, c("x", "y")], col = "#009E73")
text(Snow.deaths[noel.street, c("x", "y")], cex = 0.9, pos = 4,
     labels = Snow.deaths$case[noel.street], col = "#009E73")

title(main = paste0("Duplicate Observation (209) as Substitute", "\n",
                    "for Missing Case at 15 Noel Street"))

ols <- segmentOLS(sel)
segment.slope <- coef(ols)[2]
segment.intercept <- coef(ols)[1]
orthogonal.slope <- orthogonalSlope(sel)

orthogonal.intercept.282 <- orthogonalIntercept(282)
orthogonal.intercept.422 <- orthogonalIntercept(422)

x.out <- max(interpolatedPoints(282, 422))
y.out <- orthogonal.slope * x.out + orthogonal.intercept.282

noel.282 <- Snow.deaths[282, c("x", "y")]
noel.422 <- Snow.deaths[422, c("x", "y")]
noel.df <- data.frame(x = c(noel.282$x, noel.422$x),
                      y = c(noel.282$y, noel.422$y))

r <- dist(noel.df) * 0.5  # radius
unit.base <- 100
unit.radians <- 2 * pi / unit.base
circumference.x <- Snow.deaths[282, "x"] + r * cos(0:unit.base * unit.radians)
circumference.y <- Snow.deaths[282, "y"] + r * sin(0:unit.base * unit.radians)
lines(circumference.x, circumference.y, col = "#009E73")

x.pt <- (orthogonal.intercept.282 - segment.intercept) /
  (segment.slope - orthogonal.slope)
y.pt <- segment.slope * x.pt + segment.intercept
segments(x.pt, y.pt, x.out, y.out, col = "#009E73")

ordered.pair <- paste0("(", round(x.out[1], 3), ", ", round(y.out[1], 3), ")")

points(x.out[1], y.out[1], pch = 0, col = "red")
text(x.out[1], y.out[1], labels = paste("209", ordered.pair), col = "red",
     cex = 0.9, pos = 2)

arrows(Snow.deaths[209, "x"], Snow.deaths[209, "y"], x.out[1], y.out[1],
       col = "black", length = 1/8)

legend(x = "bottomleft",
       legend = c("Duplicate", "Case", "15 Noel St"),
       col = c("red", "black", "#009E73"),
       pch = c(0, 1, 1),
       bg = "white",
       title = "Key")

text(noel.x, noel.y, "Noel St", srt = noel.angle)
text(poland.x, poland.y, "Poland St", srt = poland.angle)

