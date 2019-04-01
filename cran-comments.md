## Test environments
* local Mac OS install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This package was removed from CRAN on ... for ... . I apologize for this---my email system 
sent the warning messages from CRAN to a Junk folder. I have fixed my email settings to 
prevent this in the future. I have resolved all ... that caused ... in the previous 
CRAN release, which resulted from updates to a package dependency.
* This package suggests a package in a non-mainstream repository (a drat repository). On the Windows build, this results in the NOTE "Package suggested but not available for checking".
* The Windows build lists some possibly mis-spelled words in DESCRIPTION. We have confirmed that these words are spelled correctly. 

## Reverse dependencies

The only reverse dependency is the `noaastormevents` package, which I also maintain. I will 
ensure that package is updated to accomodate the new version of this package (`hurricaneexposure`).
