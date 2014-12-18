##------------------------------------
#' The imgnoiser package
#'
#' Tools to measure, analyze and profile raw image noise.
#'
#' The \pkg{imgnoiser} package contains tools aimed to automatize
#' the measurement, analysis and profiling of image noise.
#'
#' @docType package
#' @name imgnoiser-package
##------------------------------------
NULL

#------------------------------------
# .onAttach
#------------------------------------
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to imgnoiser.")
}
