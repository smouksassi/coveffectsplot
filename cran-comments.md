## Test environments
* local Windows 10 install, R 4.0.2
* ubuntu 16.04.6 LTS (on travis-ci), 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a hotfix for failure communicated by Prof Ripley due to using cairo on MAC
* added vignettes and as such the note on invalid URL are for these vignettes
* some Notes from the previous builds on some platforms are false positives since these are used in the shiny app code and are required for it.

## Reverse dependencies

There is no listed dependencies.
---