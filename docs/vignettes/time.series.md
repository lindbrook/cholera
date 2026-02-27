Time Series
================
lindbrook
2026-02-27

## Overview

The standard “textbook” story is that John Snow helped stem the tide of
the cholera outbreak by getting local officials to remove the handle of
the Broad Street pump (e.g.,
[CDC](https://www.cdc.gov/mmwr/preview/mmwrhtml/mm5334a1.htm/) and [NY
Times](https://www.nytimes.com/2017/02/06/health/cholera-vaccine-bangladesh.html/)).

While Snow likely believed that this action would have mitigated the
outbreak, he was skeptical of its actual effect. In fact, he writes that
by the time the pump was removed on Friday, 08 September 1854, “the
epidemic had evidently subsided” (Snow 1855b, 153).

As part of the Vestry Report, both he and the committee included
separate tables for the number of deaths and fatalities. I have included
the four sets data in `timeSeries()`. They can be visualized using the
function’s generic plot method.

``` r
plot(timeSeries())
```

<img src="time.series_files/figure-gfm/unnamed-chunk-2-1.png" alt="" style="display: block; margin: auto;" />

The overall data approximate one another. The same statistics from the
different sources are very close.

``` r
plot(timeSeries(dataset = "all"), multi.plot = TRUE, points = FALSE)
```

<img src="time.series_files/figure-gfm/unnamed-chunk-3-1.png" alt="" style="display: block; margin: auto;" />

``` r
plot(timeSeries(dataset = c("snow-deaths", "vestry-deaths")), multi.plot = TRUE, points = FALSE)
```

<img src="time.series_files/figure-gfm/unnamed-chunk-4-1.png" alt="" style="display: block; margin: auto;" />

``` r
plot(timeSeries(dataset = c("snow-fatal.attacks", "vestry-fatal.attacks")), multi.plot = TRUE, points = FALSE)
```

<img src="time.series_files/figure-gfm/unnamed-chunk-5-1.png" alt="" style="display: block; margin: auto;" />
