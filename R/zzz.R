.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
    has_data <- requireNamespace("hurricaneexposuredata", quietly = TRUE)
    if (!has_data) {
        msg <- paste("To use this package, you must install the",
                     "hurricaneexposuredata package.\n To install that ",
                     "package, run `install.packages('hurricaneexposuredata',",
                     "repos='https://geanders.github.io/drat/', type='source')`.\n",
                     "See the `hurricaneexposure` vignette for more details.")
        packageStartupMessage(msg)
    }
    assign("has_data", has_data, envir = .pkgenv)
}

hasData <- function(has_data = .pkgenv$has_data){
    if (!has_data) {
        stop(paste("To use this function, you must have the",
                   "`hurricaneexposuredata` package installed. See the",
                   "`hurricaneexposure` package vignette for more details."))
    }
}
