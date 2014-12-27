
#------------------------------------
# .onAttach
#' @importFrom utils packageVersion
#------------------------------------
.onAttach <- function(...) {
  greeting <- paste0("Welcome to imgnoiser v",utils::packageVersion("imgnoiser"),".")
  packageStartupMessage(greeting)
}
