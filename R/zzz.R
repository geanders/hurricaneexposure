.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
    has_data <- requireNamespace("hurricaneexposuredata", quietly = TRUE)
    .pkgenv[["has_data"]] <- has_data
}
    
.onAttach <- function(libname, pkgname) {
    if (!.pkgenv$has_data) {
        msg <- paste("To use this package, you must install the",
                     "hurricaneexposuredata package. To install that ",
                     "package, run `install.packages('hurricaneexposuredata',",
                     "repos='https://geanders.github.io/drat/', type='source')`.",
                     "See the `hurricaneexposure` vignette for more details.")
        msg <- paste(strwrap(msg), collapse="\n")
        packageStartupMessage(msg)
    }
}

hasData <- function(has_data = .pkgenv$has_data) {
    if (!has_data) {
        msg <- paste("To use this function, you must have the",
                     "`hurricaneexposuredata` package installed. See the",
                     "`hurricaneexposure` package vignette for more details.")
        msg <- paste(strwrap(msg), collapse="\n")
        message(msg)
        return(invisible(NULL))
    }
}
