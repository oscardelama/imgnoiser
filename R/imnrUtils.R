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


#--------------------------------------
#
# Splits the raw image in 4 channels.
# The channel values are returned as a list of matrices.
#
# The content of each each returned channel depends on the
# content of the bottom left of the source image:
#
#  Old -> new
#   3 -> 1
#   4 -> 2
#   1 -> 3
#   2 -> 4
#   :   :  :
#  ch1 ch2 :
#  ch3 ch4 :
#
# For a Nikon raw image, cropped with an even number of
# rows and columns of pixels at the the lef and bellow to
# the crop these channels corresponds to (Gb, B, R, Gr):
#
# R  Gr
# Gb  B
#
#--------------------------------------
split_cfa <- function(.fileName, file.path) {

  # require(FITSio)
  file.name <-
    if (missing(file.path)) .fileName
  else paste0(file.path, .fileName)

  img <- FITSio::readFITS(file.name)

  # Number of rows (X axis)
  nrows <- nrow(img$imDat)
  ncols <- ncol(img$imDat)

  ch.rows <- nrows %/% 2L
  ch.cols <- ncols %/% 2L

  ch1 <- matrix( nrow = ch.rows, ncol = ch.cols)
  ch2 <- matrix( nrow = ch.rows, ncol = ch.cols)
  ch3 <- matrix( nrow = ch.rows, ncol = ch.cols)
  ch4 <- matrix( nrow = ch.rows, ncol = ch.cols)

  nrows_m1 <- ch.rows*2L - 1L
  ncols_m1 <- ch.cols*2L - 1L
  col.start <- ncols %% 2

  for (row in 0:nrows_m1) {
    for (col in col.start:ncols_m1) {

      chn <- (row %% 2) + 2*(col %% 2)
      chn_row <- row %/% 2 + 1
      chn_col <- col %/% 2 + 1

      switch(
        EXPR = chn+1
        ,{ch1[chn_row, chn_col] = img$imDat[row+1, col+1]}
        ,{ch2[chn_row, chn_col] = img$imDat[row+1, col+1]}
        ,{ch3[chn_row, chn_col] = img$imDat[row+1, col+1]}
        ,{ch4[chn_row, chn_col] = img$imDat[row+1, col+1]}
      )
    }
  }
  return (list("ch1"= ch3, "ch2"= ch4, "ch3"= ch1, "ch4"= ch2))
}

# split_cfa <- function(.fileName, file.path) {
#
#   # require(FITSio)
#   file.name <-
#     if (missing(file.path)) .fileName
#   else paste0(file.path, .fileName)
#
#   img <- FITSio::readFITS(file.name)
#
#   # Number of rows (X axis)
#   nrows <- nrow(img$imDat)
#   ncols <- ncol(img$imDat)
#
#   ch1 <- matrix( nrow = (nrows+1) %/% 2, ncol =(ncols+1) %/% 2)
#   ch2 <- matrix( nrow = (nrows+0) %/% 2, ncol =(ncols+1) %/% 2)
#   ch3 <- matrix( nrow = (nrows+1) %/% 2, ncol =(ncols+0) %/% 2)
#   ch4 <- matrix( nrow = (nrows+0) %/% 2, ncol =(ncols+0) %/% 2)
#
#   nrows_m1 = nrows - 1
#   ncols_m1 = ncols - 1
#
#   for (row in 0:nrows_m1) {
#     for (col in 0:ncols_m1) {
#
#       chn <- (row %% 2) + 2*(col %% 2)
#       chn_row <- row %/% 2 + 1
#       chn_col <- col %/% 2 + 1
#
#       switch(
#         EXPR = chn+1
#         ,{ch1[chn_row, chn_col] = img$imDat[row+1, col+1]}
#         ,{ch2[chn_row, chn_col] = img$imDat[row+1, col+1]}
#         ,{ch3[chn_row, chn_col] = img$imDat[row+1, col+1]}
#         ,{ch4[chn_row, chn_col] = img$imDat[row+1, col+1]}
#       )
#     }
#   }
#   return (list("ch1"= ch3, "ch2"= ch4, "ch3"= ch1, "ch4"= ch2))
# }

#-------------------------------------
# valid.option
#-------------------------------------
channelVar <- function(.v) {
  return (var(c(.v)))
}

#-------------------------------------
# valid.option
#-------------------------------------
channelMean <- function(.v) {
  return (mean(c(.v)))
}

#-------------------------------------
# valid.option
#-------------------------------------
channelCov <- function(.v1, .v2) {
  return (cov(c(.v1), c(.v2)))
}

#-------------------------------------
# valid.option
#-------------------------------------
print_model_summary <- function(model, model.name, channel.labels) {

  writeLines('##-------')
  writeLines(paste("## Model Name: ", sQuote(model.name)))
  writeLines('##-------\n')

  for (ix in seq_len(length(model))) {
    cat("#-- Channel :", channel.labels[ix],"--#\n")
    print(summary(model[[ix]]))
  }
}

#-----------------------------
# @Pure functional
# function expand.img.file.names(img.file.name, img.file.count)
#-----------------------------
expand_img_file_names <- function(img.file.name, img.file.count, img.file.name.ext, file.path) {

  img.file.name <- as.character(vector.alike(img.file.name, 1L, Inf))
  img.file.count <- vector.alike(img.file.count, 1L, type='i')
  img.file.name.ext <- vector.alike(img.file.name.ext, 1L)

  if (!grepl('(\\.)?(fit|fits)$', img.file.name.ext, ignore.case=TRUE))
    stop("Unsupported file name extension.")

  if (!is.whole.number(img.file.count))
    stop("The 'img.file.count' argument is not integer.")

  # The name extension begins with '.'
  if (!grepl('^\\.', img.file.name.ext))
    img.file.name.ext <- paste0('.', img.file.name.ext)

  all.file.names <- rep("", times = img.file.count);
  for (name.ix in 1L:img.file.count)
    all.file.names[name.ix] <- paste0(img.file.name, name.ix, img.file.name.ext)

  all.file.names;

}

#-----------------------------
# @Pure functional
# function expand.img.file.names(img.file.name, img.file.count)
#-----------------------------
map.col.names <- function(df, col.map) {

  # Keep only the entries where the key is different to the value
  col.map <- col.map[names(col.map) != unlist(col.map)]

  if (length(col.map) != 0) {
    df.name <- names(df)

    for(col_ix in seq_along(col.map)) {
      current.name <- names(col.map)[col_ix]
      expected.name <- col.map[[current.name]]

      if (current.name %in% df.name)
        df.name[df.name == current.name] <- expected.name
    }
    names(df) <- df.name
  }

  return(df)
}
