####################################################################################
# This script includes the example R code from Anderson and Eddelbuettel, 'Hosting
# Data Packages via `drat`: A case study with hurricane exposure data.
####################################################################################

# Build a package into a temporary directory. This code should be called when working in
# the directory of the package to be built.
tmp <- tempdir()
devtools::build(path = tmp)

# Insert the package into a repository named 'drat' located in '~/git'
pkg_path <- file.path(tmp, "hurricaneexposuredata_0.0.2.tar.gz", sep = "/")
drat::insertPackage(pkg_path, commit = TRUE)

# Code to include in the code package's `zzz.R` file
.pkgenv <- new.env(parent=emptyenv())                                             #1

.onLoad  <- function(libname, pkgname) {                                          #2
  has_data <- requireNamespace("hurricaneexposuredata", quietly = TRUE)         #3
  .pkgenv[["has_data"]] <- has_data                                             #4
}

.onAttach <- function(libname, pkgname) {                                         #5
  if (!.pkgenv$has_data) {                                                      #6
    msg <- paste("To use this package, you must install the",
                 "hurricaneexposuredata package. To install that ",
                 "package, run `install.packages('hurricaneexposuredata',",
                 "repos='https://geanders.github.io/drat/', type='source')`.",
                 "See the `hurricaneexposure` vignette for more details.")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
}

hasData <- function(has_data = .pkgenv$has_data) {                                #7
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`hurricaneexposuredata` package installed. See the",
                 "`hurricaneexposure` package vignette for more details.")
    msg <- paste(strwrap(msg), collapse="\n")
    stop(msg)
  }
}

# Code to add at the beginning of the vignette(s) of the code package to ensure vignette
# code is conditioned to only run if the data package is available
hasData <- requireNamespace("hurricaneexposuredata", quietly = TRUE)                #1
if (!hasData) {                                                                     #2
  knitr::opts_chunk$set(eval = FALSE)                                             #3
  msg <- paste("Note: Examples in this vignette require that the",
               "`hurricaneexposuredata` package be installed. The system",
               "currently running this vignette does not have that package",
               "installed, so code examples will not be evaluated.")
  msg <- paste(strwrap(msg), collapse="\n")
  message(msg)                                                                    #4
}

# Code to use within examples in the help documentation of the code package to
# condition the examples to only run if the data package is available
if (requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
  # Example code to run
}
