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


#----------------------------------
#' Load imoiser objects
#'
#' Load an instance of a imgnoiser object saved in a specified file. An instance
#' of the \code{imgnoiser} classes \code{\link{hvdvm}},
#' \code{\link{vvm}} is loaded from the a given file.
#'
#' @usage
#'   imgnoiser.load(
#'      file.name    = stop("A 'file.name' argument is required."),
#'      load.options = imgnoiser.option('obj.load.options')
#'   )
#'
#' @param file.name Name of the file containing the object that will be loaded.
#'   This is file previously saved with the \code{\link{imgnoiser.save}}
#'   function.
#'
#' @param load.options If TRUE and if the the \code{imgnoiser} options were
#'   saved in the file, those options will be retrieved from the file and set as
#'   the current ones.
#'
#' @return The object read from the \code{file.name} file.
#'
#' @examples
#' \dontrun{
#'
#' # Retrieve the my.hvdvm object previously saved with imgnoiser.save
#' my.hvdvm <- imgnoiser.load('my.hvdvm')
#' #> An instance of the "hvdvm" was succesfully read from the "my.hvdvm.imn" file.
#' }
#'
#' @seealso \code{\link{imgnoiser.save}}
#' @export
#----------------------------------
imgnoiser.load <- function(
     file.name    = stop("A 'file.name' argument is required.")
    ,load.options = imgnoiser.option('obj.load.options')
  ) {

  # Validate arguments
  file.name <- vector.alike(file.name, 1L)
  load.options <- vector.alike(load.options, 1L, type='l')
  # Add file name extension if required
  file.name <- with.file.name.extension(file.name)

  # Check file existence
  if (!file.exists(file.name))
    stop('The file ', dQuote(file.name), ' does not exists.')

  # Check the file is readable
  if (file.access(file.name, 4) != 0)
    stop('Cannot red the ', dQuote(file.name), ' file.')

  bag <- readRDS(file.name)

  # Get and validate the object class in the bag
  class.name <- bag[['object.class']]
  if (is.null(class.name) | !any(class.name %in% c('hvdvm', 'vvm')))
    stop('The ', dQuote(file.name), ' file does not contains an imgnoiser object.')
  else
    bag[['object.class']] <- NULL

  # Load the options if possible and required
  if (load.options) {
    all.package.options <- bag[['imgnoiser.options']]
    if (is.null(all.package.options))
      warning('The ', dQuote(file.name), ' file doe not contain the package options.')
    else {
      options('imgnoiser' =  all.package.options)
      bag[['imgnoiser.options']] <- NULL
    }
  }

  #-- Start the object upacking
  if (any(class.name %in% 'hvdvm'))
    obj <- hvdvm$new()
  else
    obj <- vvm$new()

  obj$.unpack(bag)
  bag <- NULL

  msg('An instance of the ', dQuote(class.name[1]), ' class was successfully load from the ', dQuote(file.name), ' file.')

  # Return the object
  invisible(obj);
}

#----------------------------------
#' Save imoiser objects
#'
#' Save an instance of a \code{imgnoiser} object to the specified file. An
#' instance of the classes \code{\link{hvdvm}}, \code{\link{vvm}}
#' with all its internal data and models is saved to a file with a given name.
#'
#' @usage
#'   imgnoiser.save(
#'      obj            = stop("An 'obj' argument is required."),
#'      file.name      = deparse(substitute(obj)),
#'      save.options   = imgnoiser.option('obj.save.options'),
#'      stop.overwrite = imgnoiser.option('stop.save.overwrite')
#'    )
#'
#' @param obj The \code{imgnoiser} object that will be saved.
#'
#' @param file.name Name of the file where the \code{obj} will be saved. If this
#'   argument does not contains '.imn' as file name extension, it will be
#'   appended to it.
#'
#' @param save.options If TRUE all the \code{imgnoiser} options and the \code{obj}
#'   object will be saved together in the \code{file.name} file.
#'
#' @param stop.overwrite If TRUE will only allow to save the \code{obj} in a new
#'   file. The function will not overwrite a file an already existing
#'   \code{file.name} file. If FALSE, will silently overwrite a file with the
#'   same name if it already exists.
#'
#' @examples
#' \dontrun{
#'
#' # Save the my.hvdvm object, with his own name "my.hvdvm"
#' imgnoiser.save(my.hvdvm)
#' #> The "my.hvdvm" object was succesully saved in the "my.hvdvm.imn" file.
#'
#' # Try to save it again, with imgnoiser.option('stop.save.overwrite') set to FALSE
#' imgnoiser.save(my.hvdvm)
#' #> Error: The file "my.hvdvm.imn" already exists.
#'
#' # Force the overwriting
#' imgnoiser.save(my.hvdvm, stop.overwrite = FALSE)
#' #> The "my.hvdvm" object was succesully saved in the "my.hvdvm.imn" file.
#'
#' # Save once again, this time with a given file name
#' imgnoiser.save(my.hvdvm, "my.hdvm.backup")
#' #> The "my.hvdvm" object was succesully saved in the "my.hdvm.backup.imn" file.
#' }
#'
#' @seealso \code{\link{imgnoiser.load}}
#' @export
#----------------------------------
imgnoiser.save <- function(
     obj            = stop("The 'obj' argument is missing.")
    ,file.name      = deparse(substitute(obj))
    ,save.options   = imgnoiser.option('obj.save.options')
    ,stop.overwrite = imgnoiser.option('stop.save.overwrite')
) {

  # Get the imgnoiser class name
  class.name <- class(obj)
  class.name <- class.name[class.name %in% c('hvdvm', 'vvm')]
  # Validate: obj is a imgnoiser class
  if (length(class.name) == 0)
    stop("The 'obj' argument must be an 'hvdvm' or 'vvm' class.")

  # Validate other arguments
  file.name <- vector.alike(file.name, 1L)
  save.options <- vector.alike(save.options, 1L, type='l')
  stop.overwrite <- vector.alike(stop.overwrite, 1L, type='l')
  #If required add the 'package'.imn' filename extension
  file.name <- with.file.name.extension(file.name)

  # Check overwriting
  if (file.exists(file.name) & stop.overwrite == TRUE)
    stop('The file ', dQuote(file.name), ' already exists.')

  # Object name for the final message
  obj.name <- deparse(substitute(obj))

  #-- Prepare and save the bag
  bag <- list()
  # Save the class name
  bag[['object.class']] <- class.name

  if (save.options)
    bag[['imgnoiser.options']] <- options('imgnoiser')[[1L]]

  saveRDS(obj$.pack(bag), file.name)
  msg('The ', dQuote(obj.name), ' object was successully saved in the ', dQuote(file.name), ' file.')
}


#----------------------------------
# with.file.name.extension
#----------------------------------
with.file.name.extension <- function(file.name) {
  if (grepl('\\.imn$', file.name, ignore.case = TRUE))
    file.name
  else
    if (grepl('\\.$', file.name, ignore.case = TRUE))
      paste0(file.name,'imn')
  else
    paste0(file.name,'.imn')
}
