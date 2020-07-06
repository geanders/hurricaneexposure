## Test environments
* local Mac OS install, R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This package suggests a package in a non-mainstream repository (a drat
repository). On the Windows build, this results in two NOTES, the NOTE "Package
suggested but not available for checking" as well as a note for "Suggests or
Enhances not in mainstream repositories".

## Reverse dependencies

The only reverse dependencies are the `noaastormevents` and `countyfloods`
packages, which I also maintain or am a coauthor of. I will ensure those
packages are updated to accomodate the new version of this package
(`hurricaneexposure`).
