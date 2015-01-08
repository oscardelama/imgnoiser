#-----------------------------------------------------------------------------
#     imgnoiser R package
#     Copyright (C) 2014  Oscar de Lama del Rio
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to the Free Software Foundation, Inc.,
#     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#' Split a raw image to get the four channels
#'
#' @param file.name The file name of the raw image. It must have the extension
#' '.fit' or '.pg'.
#'
#' @param file.path The path to the file referred in \code{file.name}
#'
#' @return An list of four matrices with the image from each channel.
#'
#' @export
#-----------------------------------------------------------------------------
split_channels <- function(file.name, file.path) {

  if (grepl('\\.(fit|fits)$', file.name, ignore.case=TRUE))
    file.format <- 'FIT'
  else
    if (grepl('\\.pgm$', file.name, ignore.case=TRUE))
      file.format <- 'PGM'
    else
      stop("Unsupported file name extension.")

  file.name <-
    if (missing(file.path))
      file.name
  else
    paste0(file.path, file.name)

  result <-
    switch(
      file.format
      ,'FIT' = split.cfa.fit(file.name)
      ,'PGM' = split.cfa.pgm(file.name)
      ,{stop("Unexpected type.")}
    )

  result;
}

split.cfa.fit <- function(file.name) {

  if (!requireNamespace("FITSio", quietly = TRUE)) {
    stop("Package FITSio required for this function to work. Please install it."
         ,call. = FALSE)
  }

  img <- FITSio::readFITS(file.name)
  img.matrix <- t(img$imDat)

  # Number of rows (X axis)
  nrows <- nrow(img.matrix)
  ncols <- ncol(img.matrix)
  nrows <- nrows - (nrows %% 2L)
  col.start <- (ncols %% 2L)

  odd.row.idx  <- rev(seq(1L, nrows, by=2L))
  even.row.idx <- rev(seq(2L, nrows, by=2L))
  even.col.idx <- seq(col.start, ncols, by=2L)
  odd.col.idx  <- seq(col.start+1L, ncols, by=2L)

  ch1 <- img.matrix[even.row.idx, odd.col.idx]
  ch2 <- img.matrix[even.row.idx, even.col.idx]
  ch3 <- img.matrix[odd.row.idx,  odd.col.idx]
  ch4 <- img.matrix[odd.row.idx,  even.col.idx]

  return (list("ch1"= ch1, "ch2"= ch2, "ch3"= ch3, "ch4"= ch4))
}

split.cfa.pgm <- function(file.name) {

  file.data <- read.pgm(file.name)
  img.matrix <- file.data[['image']]

  # Number of rows (X axis)
  nrows <- nrow(img.matrix)
  ncols <- ncol(img.matrix)

  # Round to even number
  nrows <- nrows - (nrows %% 2L)
  ncols <- ncols - (ncols %% 2L)

  odd.row.idx  <- seq(1L, nrows, by=2L)
  even.row.idx <- seq(2L, nrows, by=2L)
  odd.col.idx  <- seq(1L, ncols, by=2L)
  even.col.idx <- seq(2L, ncols, by=2L)

  ch1 <- img.matrix[odd.row.idx,  odd.col.idx]
  ch2 <- img.matrix[odd.row.idx,  even.col.idx]
  ch3 <- img.matrix[even.row.idx, odd.col.idx]
  ch4 <- img.matrix[even.row.idx, even.col.idx]

  return (list("ch1"= ch1, "ch2"= ch2, "ch3"= ch3, "ch4"= ch4))
}
