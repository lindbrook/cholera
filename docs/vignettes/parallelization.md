Parallelization
================
lindbrook
2025-03-12

The ‘cholera’ package supports parallelization via the ‘parallel’ R
package. It is *off* by default. To enable it, either set
`multi.core = TRUE`, which uses all logical cores, or pass the number of
cores the `multi.core` argument (e.g., `multi.core = 2`). To check the
number of physical/logical cores on your system, use
`parallel::detectCores()`.

Two things to note. First, support for Windows is currently limited.
Second, the documentation for the ‘parallel’ package discourages the use
of parallelization in the GUI: “It is strongly discouraged to use these
functions in GUI or embedded environments, because it leads to several
processes sharing the same GUI which will likely cause chaos (and
possibly crashes).” That said, with recent versions of ‘parallel’, I
have rarely, if ever experience crashes in either the R application or
RStudio.

Besides courtesy, parallelization is now off by default because coding
changes have improved performance for intensive tasks like computing and
plotting area polygons for “expected” cases in neighborhoodWalking().

## Benchmarks

The two tables below compare the timings (in seconds) for ‘cholera’
versions 0.8.0 and 0.9.0 for the following expressions:

``` r
# 1 core
plot(neighborhoodWalking())
plot(neighborhoodWalking(case.set = "expected"), type = "area.polygons")
plot(neighborhoodWalking(pump.select = 6:7, case.set = "expected"), type = "area.polygons")
```

``` r
# 4 cores
plot(neighborhoodWalking(multi.core = TRUE))
plot(neighborhoodWalking(case.set = "expected", multi.core = TRUE), type = "area.polygons")
plot(neighborhoodWalking(pump.select = 6:7, case.set = "expected", multi.core = TRUE), 
  type = "area.polygons")
```

The timings below reflect the average of 5 evaluations of the above
expressions using the ‘microbenchmark’ package, R version 4.4.3 and
macOS 13.7.4 on a 3.1 GHz Dual-Core Intel Core i5 processor.

| v\. 0.8.0                            | 1 core | 4 cores |
|:-------------------------------------|-------:|--------:|
| observed                             |    4.0 |     3.9 |
| expected; area.polygons              |  285.5 |   131.5 |
| expected; pumps 6 & 7; area.polygons |  227.5 |   121.2 |

| v\. 0.9.0                            | 1 core | 4 cores |
|:-------------------------------------|-------:|--------:|
| observed                             |    2.0 |     2.1 |
| expected; area.polygons              |   29.1 |    20.8 |
| expected; pumps 6 & 7; area.polygons |   80.3 |    57.8 |
