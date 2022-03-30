## Test environments
* local Windows 10 install, R 4.1.3
* ubuntu 20.04 LTS (on github actions), R  4.1.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a hotfix to prevent being removed from cran and it fixes a failure to retrieve data from CDC via url by including the data in the package
* some Notes regarding packages not used are false positives since these are used in the shiny app code and are required for it.
* all listed package dependencies are required for the shinyapp included in the package to work properly

## Reverse dependencies

There is no listed dependencies.
---