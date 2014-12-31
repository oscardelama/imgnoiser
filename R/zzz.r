
#------------------------------------
# .onAttach
#' @importFrom utils packageVersion
#------------------------------------
.onAttach <- function(...) {
  greeting <- paste0("Welcome to imgnoiser v",utils::packageVersion("imgnoiser"),".")
  packageStartupMessage(greeting)
}

#------------------------------------
# .onLoad
#------------------------------------
.onLoad <- function(libname, pkgname) {
  imgnoiser.options.init()
  invisible()
}

#------------------------------------
# .onUnload
#------------------------------------
.onUnload <- function(...) {

}
