.pkgglobalenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
  has_data_package <- requireNamespace("hurricaneexposuredata")
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
