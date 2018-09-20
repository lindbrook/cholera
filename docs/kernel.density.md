Kernel Density Plot
================
lindbrook
2018-09-20

Overview
--------

By default, the `addKernelDensity()` function pools all observations:

``` r
snowMap()
addKernelDensity()
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

However, the function allows you to explore different scenarios.

Defining the set of observed pump neighborhoods
-----------------------------------------------

By using the `pump.select` argument you can specify which pumps should be considered (i.e., define the "population" of pump neighborhoods).

``` r
snowMap()
addKernelDensity(pump.select = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Selecting a subset of observed neighborhoods
--------------------------------------------

By using the `pump.subset` argument, you can specify which subset of selected neighborhoods will be plotted.

``` r
snowMap()
addKernelDensity(pump.subset = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
