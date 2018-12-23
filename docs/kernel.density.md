Kernel Density Plot
================
lindbrook
2018-12-23

By default, the `addKernelDensity()` function pools all observations:

``` r
snowMap()
addKernelDensity()
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

However, the function provided two ways to explore different hypothetical scenarios. You can define the "population" of pump neighborhoods by using the `pump.select` argument to specify the pumps to be considered.

``` r
snowMap()
addKernelDensity(pump.select = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

You can define the subset of the "population" to consider by using the `pump.subset` argument,

``` r
snowMap()
addKernelDensity(pump.subset = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
