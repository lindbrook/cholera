### Test environments

* local OS X install, R 3.4.3 (2017-11-30)
* win-builder (devel and release)


### R CMD check results

3.4.3 (2017-11-30), x86_64-apple-darwin15.6.0 (64-bit):

* 0 errors | 0 warnings | 0 notes


R-devel: R Under development (unstable) (2018-01-26 r74162): x86_64-w64-mingw32 (64-bit)

* 0 errors | 0 warnings | 1 note

  - Package has a FOSS license but eventually depends on the following
    packages which restrict use:
      alphahull, tripack

  Next release will remove these dependencies.


R-release: 3.4.3 (2017-11-30): x86_64-w64-mingw32 (64-bit)

* 0 errors | 0 warnings | 1 note

  - Package has a FOSS license but eventually depends on the following
    packages which restrict use:
      alphahull, tripack

  Next release will remove these dependencies.


### Reverse dependencies

There are no reverse dependencies: 
  tools::dependsOnPkgs("cholera") returns character(0)
