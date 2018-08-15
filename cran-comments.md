### Test environments

* local OS X install, R 3.5.1 (2018-07-02)
* win-builder ("R-release", "R-devel" and "R-oldrelease")


### CRAN Package Check Results for Package 'cholera'

A few functions in 'cholera' 0.5.0 return an error with R 3.4.4. This is related to the fact that base::isFALSE() was introduced in R 3.5.0. This has been fixed in 'cholera' 0.5.1.


### R CMD check results

R 3.5.1 (2018-07-02), x86_64-apple-darwin15.6.0 (64-bit):

* 0 errors | 0 warnings | 0 notes


R-release: R 3.5.1 (2018-07-02): x86_64-w64-mingw32 (64-bit)

* 0 errors | 0 warnings | 0 notes


R-devel: R Under development (2018-08-13 r75131): x86_64-w64-mingw32 (64-bit)

* 0 errors | 0 warnings | 0 notes


R-oldrelease: R 3.4.4 (2018-03-15): x86_64-w64-mingw32 (64-bit)

* 0 errors | 0 warnings | 0 notes


### Reverse dependencies

There are no reverse dependencies:
  tools::dependsOnPkgs("cholera") returns character(0)
