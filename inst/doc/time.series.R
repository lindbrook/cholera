## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cholera)

## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------
plot(vestry.time.series$date, vestry.time.series$fatalities, type = "o",
  xlab = "Date", ylab = "fatalities")
abline(v = as.Date("1854-09-08"), col = "red", lty = "dotted")
text(vestry.time.series[vestry.time.series$date == "1854-09-08",
  c("date", "fatalities")], label = "Sep 08", cex = 0.75, col = "red", pos = 4)
title(main = "Vestry Report: Removal of the Broad Street Pump Handle")


## ---- fig.width = 6, fig.height = 6, echo = FALSE------------------------
plot(snow.time.series$date, snow.time.series$deaths, type = "o", xlab = "Date", 
  ylab = "fatalities", xlim = range(vestry.time.series$date), ylim = range(vestry.time.series$fatalities))
abline(v = as.Date("1854-09-08"), col = "red", lty = "dotted")
text(snow.time.series[snow.time.series$date == "1854-09-08",
  c("date", "deaths")], label = "Sep 08", cex = 0.75, col = "red", pos = 4)
title(main = "Snow: Removal of the Broad Street Pump Handle")

