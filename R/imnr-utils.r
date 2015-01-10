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

#-------------------------------------
# valid.option
#-------------------------------------
channelVar <- function(.v) {
  return (var(c(.v)))
}

#-------------------------------------
# valid.option
#-------------------------------------
channelMean <- function(v) mean(c(v));

#-------------------------------------
# valid.option
#-------------------------------------
channelCov <- function(v1, v2) cov(c(v1), c(v2));

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
#
#' @importFrom data.table setnames
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
    #names(df) <- df.name
    data.table::setnames(df, df.name)
  }

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

  if (!grepl('.*/$', file.path))
    file.path <- paste0(file.path, '/')

  file.path;
}

#-----------------------------
#' Query the files in a given folder
#'
#' Return a vector with the names of the files in the folder \code{path}, whose
#' names are alphabetically between \code{from} and \code{to} and have the name
#' extension given in \code{extension}.
#'
#' @param from
#' @param to The \code{from} and \code{to} are the alphabetical range of the
#'   desired files in folder given in \code{path}. These names should not
#'   include the name extension which is specified with \code{extension}.
#'
#' @param extension The file name extension common to the desired files.
#'
#' @return A character vector with the file names satisfying the selection.
#'
#' @examples
#' \dontrun{
#'
#' # Select the file in the folder './samples' whose name is between
#' # _DSC5695.NEF and _DSC5716.NEF
#' files <- select.files('_DSC5695', '_DSC5716', '.NEF', './samples')
#' }
#'
#' @export
select.files <- function(from, to, extension, path) {
  vector.alike(extension, 1)

  # Remove beginning period
  if (grepl('^\\..*', extension)) {
    if (nchar(extension) == 1)
      stop("Invalid extension.")
    else
      extension <- substr(extension, 2, nchar(extension))
  }

  regexp <- paste0('.*\\.', extension)
  files <- list.files(path, pattern=regexp)
  if (length(files) == 0)
    warning("The given folder does not contains any file with the given name extension ", sQuote(extension))
  else {
    from <- paste0(from, '.', extension)
    to <- paste0(to, '.', extension)

    files <- files[files >= from & files <= to]
    if (length(files) == 0)
      warning("There are files with the extension ", sQuote(extension), " but none of them is in the given range.")
    else
      message(length(files), " files were found.")

    files;
  }
}
