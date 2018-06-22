Kernel Density Plot
================
lindbrook
2018-06-22

Overview
--------

By default, addKernelDensity() function pools all observations:

``` r
snowMap(add.title = FALSE)
addKernelDensity()
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

But you can also view the kernel densities of individual pump neighborhoods. Either by selecting a subset of pump neighborhood or by specifying which pumps should be considered (i.e., defining the "population" of pump neighborhoods).

Selecting a subset of observed neighborhoods:
---------------------------------------------

``` r
snowMap(add.title = FALSE)
addKernelDensity(pump.subset = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Defining the set of observed pump neighborhoods:
------------------------------------------------

``` r
snowMap(add.title = FALSE)
addKernelDensity(pump.select = c(6, 8))
```

<img src="kernel.density_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
