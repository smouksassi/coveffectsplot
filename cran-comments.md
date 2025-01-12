## Test environments
* local Windows 10 install, R 4.4.2
* github actions windows-latest (release)
* github actions macOS-latest (release)
* github actions ubuntu-latest (devel, release and oldrel-1)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is to fix a bug in the plot function and to fix a vignette
* Notes about unused declared imports from the previous/current builds are false positives since these are used in the shiny app code and are required for it to work properly

## Reverse dependencies
NMsim
---