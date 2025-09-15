## Test environments
* local Windows 11 install, R 4.5.1
* github actions windows-latest (release)
* github actions macOS-latest (release)
* github actions ubuntu-latest (devel, release and oldrel-1)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a small update to make sure everything works with the latest ggplot2 (especially legends)
* Notes about unused declared imports from the previous/current builds are false positives since these are used in the shiny app code and are required for it to work properly

## Reverse dependencies
NMsim
---
