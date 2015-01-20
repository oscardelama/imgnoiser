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

is.channel <- function(ch) {
  if (length(ch) == 1 && is.logical(ch) && is.na(c(ch)[1])) FALSE
  else TRUE
}

#-------------------------------------
# valid.option
#-------------------------------------
channelVar <- function(v) {
  if (!is.channel(v))
    NA
  else
    var(c(v))
}

#-------------------------------------
# valid.option
#-------------------------------------
channelMean <- function(v) {
  if (!is.channel(v))
    NA
  else
    mean(c(v));
}

#-------------------------------------
# valid.option
#-------------------------------------
channelCov <- function(v1, v2) {
  if (!is.channel(v1) || !is.channel(v2))
    NA
  else
    cov(c(v1), c(v2));
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
#' Prepare a sequence of file names
#'
#' Return a character vector with file names following a sequence pattern.
#'
#' This is a convenience function for the case when you want to prepare a
#' vector of file names with the following pattern:
#'
#' <file.name.base><file.counter>.<file.name.ext>
#'
#' As in ('crop_1.fit', 'crop_2.fit', ... 'crop_75.fit')
#'
#' @param file.name.base The base file name, a constant prefix of the file
#'   finames.
#'
#' @param file.count The number of files in the sequence. The returning list
#'   will contain a file with file counter from 1 to up to this argument.
#'
#' @param file.name.ext The file name extension. A constant for all the file
#'   names.
#'
#' @return A character vector with \code{img.file.count} elements, with the
#'   pattern <file.name.base><file.counter>.<file.name.ext>. Where
#'   <file.counter> will vary from 1 to \code{file.count}
#'
#' @export
#-----------------------------
file.name.seq <- function(
  file.name.base,
  file.count,
  file.name.ext
) {

  file.name.base <- as.character(vector.alike(file.name.base, 1L, Inf))
  file.count <- vector.alike(file.count, 1L, type='i')
  file.name.ext <- vector.alike(file.name.ext, 1L)

  if (!grepl('(\\.)?(fit|fits)$', file.name.ext, ignore.case=TRUE))
    stop("Unsupported file name extension.")

  if (!is.whole.number(file.count))
    stop("The 'file.count' argument is not integer.")

  # The name extension begins with '.'
  if (!grepl('^\\.', file.name.ext))
    file.name.ext <- paste0('.', file.name.ext)

  all.file.names <- rep("", times = file.count);
  for (name.ix in 1L:file.count)
    all.file.names[name.ix] <- paste0(file.name.base, name.ix, file.name.ext)

  all.file.names;

}

#-----------------------------
# @Pure functional
# function expand.img.file.names(img.file.name, img.file.count)
#
#' @importFrom data.table setnames
#-----------------------------
map.col.names <- function(df, col.map) {

  df.names <- sub(' ', '.', tolower(names(df)))
  data.table::setnames(df, df.names)
  target.names <- unlist(col.map)

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
    #names(df) <- df.name
    data.table::setnames(df, df.name)
  }

  # Keep only the interesting column names
  df <- df[, which(names(df) %in% target.names)]
  return(df)
}

#-----------------------------
# Unzip the sample files
#-----------------------------
unzip.sample.files <- function(file.path) {
  files <- list.files(file.path)
  if (!all(c('crop_1.fit', 'crop_84.fit') %in% files)) {
    # create a temporary directory
    file.name <- paste0(file.path,'/samples.zip')
    unzip(file.name, exdir=file.path, overwrite=TRUE)
  }
}

#-----------------------------
# function test.data.folder()
#   Returns the folder with the raw test data
#-----------------------------
get.test.data.folder <- function() {

  file.path <- system.file('samples', package = "imgnoiser")
  unzip.sample.files(file.path)
  #   file.path <- Sys.getenv('IMNR_TEST_DATA')
  if (file.path == '')
    stop("'samples' folder not found.")

  path.with.ending.slah(file.path);
}

path.with.ending.slah <- function(file.path) {
  if (!grepl('.*/$', file.path))
    paste0(file.path, '/')
  else
    file.path
}

#-----------------------------
#' Query the files in a given folder within a given range.
#'
#' Return a vector with the names of the files in the folder
#' \code{path.to.files}, whose names are alphabetically between
#' \code{file.name.from} and \code{file.name.to} and have the name extension
#' given in \code{file.path}.
#'
#' @param file.name.from
#' @param file.name.to The \code{file.name.from} and \code{file.name.to} are the
#'   alphabetical range of the desired files from the folder given in
#'   \code{file.path}. These names should not include the file name
#'   extension, which is specified with \code{file.name.ext}.
#'
#' @param file.name.ext The file name extension common to the desired files.
#'
#' @param file.path Folder or directory with files with the name extension
#'   \code{file.name.ext} from which those with names between
#'   \code{file.name.from} and \code{file.name.to} will be returned.
#'
#' @return A character vector with the file names satisfying the selection.
#'
#' @examples
#' \dontrun{
#'
#' # Select the file in the folder './samples' whose name is between
#' # _DSC5695.NEF and _DSC5716.NEF
#' files <- select.file.range('_DSC5695', '_DSC5716', '.NEF', './samples')
#' }
#'
#' @export
#'
select.file.range <- function(
  file.name.from,
  file.name.to,
  file.name.ext,
  file.path
) {

  file.name.from <- vector.alike(file.name.from, 1L, Inf)

  if (length(file.name.from) > 1L) {
    return(file.name.from)
  }

  vector.alike(file.name.ext, 1)

  # Remove possibly beginning period
  if (grepl('^\\..*', file.name.ext)) {
    if (nchar(file.name.ext) == 1)
      stop("Invalid file.name.ext.")
    else
      file.name.ext <- substr(file.name.ext, 2, nchar(file.name.ext))
  }

  regexp <- paste0('.*\\.', file.name.ext)
  files <- list.files(file.path, pattern=regexp)
  if (length(files) == 0)
    warning("The given folder does not contains any file with the given name extension ", sQuote(file.name.ext))
  else {
    file.name.from <- paste0(file.name.from, '.', file.name.ext)
    file.name.to <- paste0(file.name.to, '.', file.name.ext)

    files <- files[files >= file.name.from & files <= file.name.to]
    if (length(files) == 0)
      warning("There are files with the name extension ", sQuote(file.name.ext), " but none of them is in the given range.")

    files;
  }
}

is.a.valid.tone.curve <- function(tc) {
  if (!is.atomic(tc) && !is.null(dim(tc)) && length(dim(tc)) == 2 && dim(tc)[2] == 2) {
    if (is.numeric(tc[,1]) & is.numeric(tc[,2])) {
      if (all(tc[,1] <= 1) & all(tc[,1] >= 0) & all(tc[,2] <= 1) & all(tc[,2] >= 0)) {
        if (nrwo(tc) > 8) return (TRUE)
      }
    }
    stop("Illegal tone curve.")
  }
  else
    FALSE
}

msg <- function(...) {
  if (imgnoiser.option('mute') != TRUE) {
    txt <- paste0(list(...), collapse="")
    message(txt)
  }
}
