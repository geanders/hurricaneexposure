.onLoad <- function(libname) {
        repos = getOption("repos")
        repos["<NAME_REPO>"] = "http://geanders.github.io/drat"
        options(repos = repos)
        invisible(repos)
}
