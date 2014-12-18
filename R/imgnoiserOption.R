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


##------------------------------
#' Get or set a imgnoiser option value
#'
#' To list all \pkg{imgnoiser} options, run this function without parameters. To
#' get one option value, call it with the option name. To set an
#' option, pass the option name and value.
#'
#' @param name A character value with the name of the option whose value you
#'   want to get or set.
#'
#'   If this argument is not provided, it will list the current value of all the
#'   \pkg{imgnoiser} options.
#'
#' @param value The value that will be set to the option with the given
#'   \code{name}
#'
#' @section Options in the imgnoiser Package:
#'
#' \subsection{avg.green.channel.label}{
#'  The label for computations made from synthesis of both green channels in a
#'  sample into only one. By default it is \code{'avg green'}.
#'
#'  For example, the mean value from the synthetized image got averaging, pixel
#'  by pixel, both green channels in a sample will be tagged with this label.
#' }
#'
#' \subsection{channel.labels}{
#' A vector of four character values to be used for the labeling of the channels
#' (Bayer color filters) in the image samples. By default, the value of this
#' option is \code{c('channel 1', 'channel 2', 'channel 3', 'channel 4')}.
#' }
#'
#' \subsection{color.pallette}{
#' A vector of five character values (or more) with the color specification for the
#' plotting of the channel values from the samples.
#'
#' The character values are in the form '#rrggbb' where rr, gg, and bb are
#' hexadecimal digits for the red greeen and blue components of the color.
#'
#' The default colors have been choose to easily distinguish one for the other
#' in the plot.
#' }
#'
#' \subsection{conds.col.map}{
#'  A \code{.csv} file with specific column names is required as input for the
#'  \code{\link{hvdvm$digest}} function. Those column names are:
#'
#'  \itemize{
#'     \item\code{crop.file.name}
#'     \item\code{lighting}
#'     \item\code{ISO}
#'     \item\code{shutter.speed}
#'     \item\code{aperture}
#'     \item\code{focal.length}
#'     \item\code{count}
#'  }
#'
#'  If the \code{.csv} file contains equivalent columns but with different
#'  naming, you can use set this option value to map from your naming to the
#'  expected one.
#'
#'  For example, if you have a column named \code{ShutterSpeed} which is not the
#'  expected \code{shutter.speed} column name, this option must contain an entry
#'  with \code{ShutterSpeed} as key and \code{shutter.speed} as value, as in
#'  \code{list('ShutterSpeed' = 'shutter.speed')}.
#' }
#'
#' \subsection{conf.factor}{
#'  Confidence factor which will be used to scale the prediction standard error
#'  in order to get the the confidence interval for each prediction.
#'
#'  For example, if the value of this argument is 2 the confidence interval for
#'  each prediction will be \code{+/- 2 * se.fit} where \code{se.fit} is the
#'  prediction standard error.
#'
#'  This option value is, by default, returned by the
#' \code{\link{imgnoiser.get.model.source.data}} function in the
#' \code{confid.factor} element.
#' }
#'
#' \subsection{fit.model}{
#'  A function allowing the fitting of data using a given model family.
#'
#'  The default value for this option is \code{\link{imgnoiser.fit.model}}.
#'
#'  The value of this option can be changed to refer to another function
#'  supporting the fitting for additional model families. This other function
#'  must have the same signature as the default function. Such new function
#'  might call the default one for the model families already implemented there.
#' }
#'
#' \subsection{fit.model.data}{
#'  A single character value with the name of the default data model.
#'
#'  During the noise analysis, the data extracted from the image samples may be
#'  analyzed from different perspectives. This way, particular variables can be
#'  directly analyzed or after some transformation. For example, comparing the
#'  variance with the mean, or maybe by consolidating the variance and
#'  covariances from all channels into one a single value and comparing it with
#'  the mean.
#'
#'  The particular set of variables under analysis are called here \emph{model
#'  data}. That set can be studied by plotting it or fitting a model over it. To
#'  find more information about this, check the
#'  \code{\link{imgnoiser.get.model.source.data}} function.
#'
#'  For the functions where a model data name is required, but not provided as
#'  argument, this name will be used. By default, the value of this
#'  option is \code{'standard'}.
#' }
#'
#' \subsection{fit.model.family}{
#'  A single character value with the name of the default fitting model.
#'
#'  For the noise analysis and study, this package allows by default the use of
#'  different fitting procedures, as linear models like ordinary least squares
#'  through the use of the \code{stats::lm} function or non-linear models as
#'  splines through the use f the \code{stats::smooth.spline} function. We call
#'  them \emph{model families}. To find more information about this, check the
#'  \code{\link{imgnoiser.fit.model}} function.
#'
#'  For the functions where a model family is required, but not provided as
#'  argument, this name will be used. By default, the value of this option is
#'  \code{'weighted'}.
#' }
#'
#' \subsection{fit.model.name}{
#'  A single character value with the name of the default fitted model.
#'
#'  During the noise analysis, a given model family (check the
#'  \code{\link{fit.model.family}} option) can be fit over a given data model
#'  (check the \code{\link{fit.model.data}} option) with some specific fitting
#'  parameters. This particular combination can be saved with a given name which
#'  is called the \emph{model name}.
#'
#'  For example a weighted robust linear model can be fitted over the mean as
#'  predictor of the variance, using as weight the inverse of the squared mean.
#'  To find more information about this, check the \code{fit.model} function in
#'  the \code{hvdvm} or the \code{vvm} class.
#'
#'  For the functions where a model name is required, but not provided as
#'  argument, this name will be used. By default, the value of this option is
#'  \code{'Model 01'}.
#' }
#'
#' \subsection{get.model.src.data}{
#'  A function returning the particular data corresponding to a given model name.
#'
#'  The default value for this option is \code{\link{imgnoiser.get.model.source.data}}.
#'
#'  The value of this option can be changed to refer to another function
#'  providing additional data models. This other function must have the same
#'  signature as the default function. Such new function might call the default
#'  one for the data models already implemented there.
#' }
#'
#' \subsection{green.channels}{
#'  A vector of two integers, aiming to positions in the \code{channel.labels}
#'  vector, marking them as the green channels. Its default value is \code{NA}.
#' }
#'
#' \subsection{has.RGGB.pattern}{
#'  A logical value indicating if the image samples has the RGGB pattern for the
#'  photosites colors.
#'
#'  This is the default value for the argument with the same name for the
#'  creation of the \code{hvdvm} and \code{vvm} class.
#' }
#'
#' \subsection{model.prediction}{
#'  A function returning the values predicted by a fitted model from a given
#'  model family.
#'
#'  The default value for this option is \code{\link{imgnoiser.model.prediction}}.
#'
#'  The value of this option can be changed to refer to another function
#'  providing the prediciton for additional model families. This other function
#'  must have the same signature as the default function. Such new function
#'  might call the default one for the prediction from model families already
#'  implemented there.
#' }
#'
#' \subsection{plot.point.size}{
#'  The point size that will be using in the plottings.
#'
#'  This value is used as argument for the \code{size} parameter in each call to
#'  the \code{ggplot2::geom_point} function.
#' }
#'
#' \subsection{plot.line.width}{
#'  The line width that will be using in the plottings.
#'
#'  This value is used as argument for the \code{size} parameter in each call to
#'  the \code{ggplot2::geom_line} function.
#' }
#'
#' \subsection{RGGB.channel.labels}{
#'  A vector of four character values with the channel labels for image samples
#'  with the RGGB pattern as channel layout. By default, the value of this option
#'  is \code{c('Red', 'Green R', 'Green B', 'Blue')}.
#' }
#'
#' \subsection{show.progress}{
#'  A single logical value indicating if a progress bar must be shown along the
#'  processing time of some functions. By default, the value of this option is
#'  \code{TRUE}
#'
#'  Some functions, as \code{\link{hvdvm$digest}} and \code{\link{vvm$digest}},
#'  must process the information during few seconds before to end. You can left
#'  this option set to \code{TRUE} to see the function progress or set it to
#'  \code{FALSE} to turn it off.
#' }
#'
#' \subsection{show.signif.stars}{
#'  A single logical value indicating if the stars (asterisks) should be printed
#'  besides the fitted model coefficients when they are showed in the summary of
#'  a fitted linear model. By default, its value is \code{FALSE}.
#'
#'  This option has identical role as the base \code{show.signif.stars} option,
#'  but affects only the model sumaries shown by the \pkg{imgnoiser} package.
#' }
#'
#' @examples
#' # Get all the options in the imgnoiser package
#' imgnoiser.option()
#'
#' # Get the default linear model
#' imgnoiser.option('default.fit.model')
#'
#' # Get the current show.progress option value
#' old.show.progress <- imgnoiser.option('show.progress')
#' # Turn off the show.progress option
#' imgnoiser.option('show.progress', FALSE)
#' # Show the show.progress option is off
#' imgnoiser.option('show.progress')
#' # Reset the show.progress option with the previous value
#' set.imgnoiser.option('show.progress', old.show.progress)
#'
#' @name imgnoiser.option
#' @export
##------------------------------
imgnoiser.option <- function(name, value) {

  all.options <- base::options('imgnoiser')[[1L]]
  if (missing(value)) {
    if (missing(name))
      all.options
    else
      all.options[[name]]
  }
  else {
    if (missing(name)) stop('Missing option name.')
    if (name %nin% names(all.options)) stop('Unknown option name.')
    all.options[[name]] <- value
    options('imgnoiser' = all.options)
  }
}

##------------------------------
#' Save the package options
#'
#' Save all the package options in a file.
#'
#' @usage
#'   imgnoiser.options.save <- function(
#'      file.name = stop("A 'file.name' argument is required."),
#'      stop.overwrite = imgnoiser.option('stop.save.overwrite')
#'      )
#'
#' @param file.name The name of the file where the options will be saved. If
#'   this file name does not already contains '.imn' as file name extension, it
#'   will be appended to it.
#'
#' @param stop.overwrite If TRUE will only allow to save the options in a new
#'   file. The function will not overwrite a file an already existing
#'   \code{file.name} file. If FALSE, will silently overwrite a file with the
#'   same name if it already exists.
#'
#' @examples
#' # Save into 'my.options' file.
#' imgnoiser.options.save('my.options')
#'
#' @seealso \code{\link{imgnoiser.options.load}}, \code{\link{imgnoiser.save}},
#'   \code{\link{imgnoiser.load}}
#'
#' @export
##------------------------------
imgnoiser.options.save <- function(
     file.name = stop("A 'file.name' argument is required.")
    ,stop.overwrite = imgnoiser.option('stop.save.overwrite')
    ) {

  # Validate arguments
  file.name <- vector.alike(file.name, 1)
  #If required add the 'package'.imn' filename extension
  file.name <- with.file.name.extension(file.name)
  stop.overwrite <- vector.alike(stop.overwrite, 1, type='l')

  if (file.exists(file.name) & stop.overwrite)
    stop('The file ', dQuote(file.name), ' already exists.')

  all.options <- base::options('imgnoiser')[[1L]]
  saveRDS(all.options, file.name)
  message('The options were succesully saved in the ', dQuote(file.name), ' file.')
}

##------------------------------
#' Load the package options
#'
#' Load the package options from a file.
#'
#' @usage
#'   imgnoiser.options.load(
#'      file.name    = stop("A 'file.name' argument is required.")
#'      )
#'
#' @param file.name Name of the file containing the options that will be loaded.
#'   This is file previously saved with the \code{\link{imgnoiser.save}}
#'   function.
#'
#' @seealso \code{\link{imgnoiser.options.save}}, \code{\link{imgnoiser.save}},
#'   \code{\link{imgnoiser.load}}
#'
#' @examples
#' \dontrun{
#'
#' # Load 'my.options'
#' imgnoiser.options.load('my.options')
#' }
#'
#' @export
##------------------------------
imgnoiser.options.load <- function(
    file.name    = stop("A 'file.name' argument is required.")
) {

  # Validate arguments
  file.name <- vector.alike(file.name, 1)
  # Add file name extension if required
  file.name <- with.file.name.extension(file.name)

  # Check file existence
  if (!file.exists(file.name))
    stop('The file ', dQuote(file.name), ' does not exists.')

  # Check the file is readable
  if (file.access(file.name, 4) != 0)
    stop('Cannot red the ', dQuote(file.name), ' file.')

  all.options <- readRDS(file.name)
  options('imgnoiser' =  all.options)
  message('The options were succesully retireved from the ', dQuote(file.name), ' file.')
}

#------------------------------------
# .onAttach
#------------------------------------
.onLoad <- function(libname, pkgname) {

  photo.conds.col.map <- list(
    'crop.file.name'  = 'crop.file.name'
    ,'lighting'        = 'lighting'
    ,'ISO'             = 'ISO'
    ,'shutter.speed'   = 'shutter.speed'
    ,'aperture'        = 'aperture'
    ,'focal.length'    = 'focal.length'
  )

  all.package.options <- list(
     'fit.model.family'     = 'lm'
    ,'fit.model.data'       = 'std.var'
    ,'fit.model.name'       = 'standard'
    ,'plot.line.width'      = 1
    ,'plot.point.size'      = 1.75
    ,'confid.factor'        = 1.96
    ,'show.progress'        = TRUE
    ,'show.signif.stars'    = FALSE
    ,'color.pallette'       = c('#E41A1C','#307AB8','#2C972E','#984EA3', '#E17E25')
    ,'channel.labels'       = c('channel 1', 'channel 2', 'channel 3', 'channel 4')
    ,'has.RGGB.pattern'     = FALSE
    ,'RGGB.channel.labels'  = c('Red', 'Green R', 'Green B', 'Blue')
    ,'green.channels'       = NA
    ,'avg.green.channel.label' = 'Green Avg'
    ,'conds.col.map'        = photo.conds.col.map
    ,'fit.model'            = imgnoiser.fit.model
    ,'get.model.predictions' = imgnoiser.model.predictions
    ,'get.model.src.data'   = imgnoiser.get.model.source.data
    ,'is.linear.model'      = c('lm', 'lmrob')
    ,'obj.save.options'     = TRUE
    ,'obj.load.options'     = TRUE
    ,'stop.save.overwrite'  = TRUE
  )

  current.package.options <- getOption('imgnoiser')
  if (is.null(current.package.options))
    current.package.options <- all.package.options
  else {
    #---------------------
    # For testing Only ===> Remove for production
    options('imgnoiser' =  all.package.options)
    return(invisible())
    #--------------------

    missing.options <- !(names(all.package.options) %in% names(current.package.options))
    if (any(missing.options)) {
      if (any(!missing.options))
        current.package.options <- c(
          current.package.options[!missing.options]
          ,all.package.options[missing.options]
        )
      else
        current.package.options <- all.package.options
    }
    else
      return(invisible())
  }

  options('imgnoiser' =  current.package.options)
  invisible()
}
