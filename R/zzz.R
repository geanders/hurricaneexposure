.pkgglobalenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
  has_data_package <- requireNamespace("hurricaneexposuredata", quietly = TRUE)
  if(!has_data_package){
    packageStartupMessage(paste("To use this package, you must install the",
                                "hurricaneexposuredata package."))
    packageStartupMessage(paste("To install that package, run",
                                "`install.packages('hurricaneexposuredata',",
                                "repos = 'https://geanders.github.io/drat/',",
                                "type = 'source')`."))
    packageStartupMessage("See the `hurricaneexposure` vignette for more details.")
  }
  assign("has_data", has_data_package, envir = .pkgglobalenv)
}

hasData <- function(has_data = .pkgglobalenv$has_data){
        if(!has_data){
                message(paste("To use this function, you must have the",
                           "`hurricaneexposuredata` package installed.\n See the",
                           "`hurricaneexposure` package vignette for more details."))
                return(NULL)
        }
}
