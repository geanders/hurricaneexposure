.onLoad <- function(libname, pkgname) {
        repos <- getOption("repos")
        repos["my_repo"] <- "http://geanders.github.io/drat"
        options(repos = repos)
        invisible(repos)
}
