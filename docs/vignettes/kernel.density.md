Kernel Density Plot
================
lindbrook
2019-01-10

By default, the `addKernelDensity()` function pools all observations:

``` r
snowMap()
addKernelDensity()
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

However, this presuppose that all cases have a common source. To consider the possible existence of multiple pump neighborhoods, the function provides two ways to explore hypothetical scenarios.

By using the `pump.select` argument, you can define a "population" of pump neighborhoods by specify the pumps to consider:

``` r
snowMap()
addKernelDensity(pump.select = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

By using the `pump.subset` argument, you can define the subset of the "population" to consider:

``` r
snowMap()
addKernelDensity(pump.subset = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
