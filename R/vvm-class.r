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
#' @include noise-class.r
#' @importFrom R6 R6Class
#' @importFrom data.table rbindlist
#' @export
#-----------------------------------------------------------------------------
vvm <- R6::R6Class('vvm', inherit = noise.var,

  public = list(

    ##------------------------------
    ## pack  (documented here)
    ##------------------------------
    .pack = function(bag) super$.pack(bag)

    ##------------------------------
    ## unpack  (documented here)
    ##------------------------------
    ,.unpack = function(bag) super$.unpack(bag)

    ##------------------------------
    ## load
    ##------------------------------
    ,digest = function(
         img.file.name = stop("The 'img.file.name' argument is required")
        ,img.file.count = NULL
        ,file.path = './'
        ,img.file.name.ext = '.fit'
    )
    {
      # browser();
      file.names <- get.img.file.names(img.file.name, img.file.count, img.file.name.ext, file.path)

      # Placeholders for the resulting data
      cov.df <- data.frame()
      var.df <- data.frame()

      known.greens <- (!is.null(private$.green.channels) & length(private$.green.channels) == 2)

      # Get show.progress option
      show.progress <- package.option('show.progress')
      # Show progress bar
      if (show.progress) {
        message(paste("Processing", length(file.names), "image files:"))
        prog.bar <- txtProgressBar(min = 0, max = length(file.names), style = 3)
      }

      for (file.ix in seq_along(file.names)) {

        file.name <- file.names[file.ix]

        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, file.ix)

        cfa <- split.cfa(file.name, file.path)

        # Build a sythetic channel with the average of both green channels
        if (known.greens) {
          chA <- array(0, dim = c(dim(cfa$ch4),2L))
          chA[,,1L] <- cfa[[private$.green.channels[1L]]]
          chA[,,2L] <- cfa[[private$.green.channels[2L]]]
          cfa$chA <- apply(chA, c(1L,2L), mean)
        }

        cov.green.avg <- function(ch1, ch2) {
          if (known.greens) channelCov(ch1, ch2) else NA
        }

        mean.ch1 <- channelMean(cfa$ch1)
        mean.ch2 <- channelMean(cfa$ch2)
        mean.ch3 <- channelMean(cfa$ch3)
        mean.ch4 <- channelMean(cfa$ch4)
        mean.chA <- if (known.greens) channelMean(cfa$chA) else NA

        var.ch1 <- channelVar(cfa$ch1)
        var.ch2 <- channelVar(cfa$ch2)
        var.ch3 <- channelVar(cfa$ch3)
        var.ch4 <- channelVar(cfa$ch4)
        var.chA <- if (known.greens) channelVar(cfa$chA)*2 else NA

        cov.ch12 <- channelCov(cfa$ch1, cfa$ch2)
        cov.ch13 <- channelCov(cfa$ch1, cfa$ch3)
        cov.ch14 <- channelCov(cfa$ch1, cfa$ch4)
        cov.ch1A <- cov.green.avg(cfa$ch1, cfa$chA)

        cov.ch23 <- channelCov(cfa$ch2, cfa$ch3)
        cov.ch24 <- channelCov(cfa$ch2, cfa$ch4)
        cov.ch2A <- cov.green.avg(cfa$ch2, cfa$chA)

        cov.ch34 <- channelCov(cfa$ch3, cfa$ch4)
        cov.ch3A <- cov.green.avg(cfa$ch3, cfa$chA)

        cov.ch4A <- cov.green.avg(cfa$ch4, cfa$chA)

        var.df <- data.table::rbindlist(list(
          var.df,
          data.frame(
             "pict" = file.name
            ,"channel" = factor(c(1L,2L,3L,4L,5L), levels=c(1L,2L,3L,4L,5L), labels=self$channel.labels)
            ,"mean" = c(mean.ch1, mean.ch2, mean.ch3, mean.ch4, mean.chA)
            ,"var" = c(var.ch1, var.ch2, var.ch3, var.ch4, var.chA)
            ,row.names = NULL
          )
        ))

        cov.df <- data.table::rbindlist(list(
          cov.df,
          data.frame(
             "pict" = file.name
            ,"chan.a" = factor(c(1L,1L,1L,1L,2L,2L,2L,3L,3L,4L), levels=c(1L,2L,3L,4L,5L), labels=self$channel.labels)
            ,"chan.b" = factor(c(2L,3L,4L,5L,3L,4L,5L,4L,5L,5L), levels=c(1L,2L,3L,4L,5L), labels=self$channel.labels)
            ,"cov.a.b" = c(cov.ch12, cov.ch13, cov.ch14, cov.ch1A, cov.ch23,
                       cov.ch24, cov.ch2A, cov.ch34, cov.ch3A, cov.ch4A)
            ,row.names = NULL
          )
        ))
      }

      # remove NAs
      # browser()
      var.df <- var.df[!is.na(var.df$mean),]
      cov.df <- cov.df[!is.na(cov.df$cov),]

      # Save the results
      private$.var.df <- var.df
      private$.cov.df <- cov.df

      # Get and save the standard model
      get.model.src.data.func <- imgnoiser.option('get.model.src.data')
      model.src.data <- get.model.src.data.func('std.var', self)
      private$.std.src.data <- model.src.data

      if (!show.progress)
        message(paste(length(file.names), "image samples were successfully processed."))
    }

    ,digest.rgb = function(
      img.file.name = stop("The 'img.file.name' argument is required")
      ,img.file.count = NULL
      ,file.path = './'
      ,img.file.name.ext = '.fit'
      ,map.to.rgb = NULL
      ,rgb.scale = 255
      ,rgb.labels = imgnoiser.option('rgb.labels')
      # This argument can be equal to 'linear' to avoid toning
      # otherwise will expect a tone curve in the camera metadata
      ,tone.curve.id = imgnoiser.option('tone.curve.id')
    )
    {
      # Validate RGGB.indices
      if (is.null(self$RGGB.indices))
        stop("The channels color are not known, set the 'RGGB.indices' argument at the object creation.")

      # Validate green channels
      if (is.null(private$.green.channels) | length(private$.green.channels) != 2)
        stop("The green channels indices are not known, set the 'green.channels' argument at the object creation.")

      # Get and validate the conversion matrix
      linear.rgb.from.raw <- map.to.rgb$linear.rgb.from.raw
      if (is.null(linear.rgb.from.raw))
        stop("The conversion matrix in the 'map.to.rgb' object is null.")

      # Validate rgb.scale
      vector.alike(rgb.scale, 1L, type='n', valid.range=c(1, 2L^16L))

      # Validate the RGB labels
      vector.alike(rgb.labels, 3L)

      # Validate and get file names of samples
      file.names <- get.img.file.names(img.file.name, img.file.count, img.file.name.ext, file.path)

      # Placeholders for the resulting data
      cov.df <- data.frame()
      var.df <- data.frame()

      # Initialize the RGB conversion
      map.to.rgb$prepare.to.rgb.conversions(rgb.scale, self$RGGB.indices, tone.curve.id)

      # Get show.progress option
      show.progress <- package.option('show.progress')
      # Show progress bar
      if (show.progress) {
        message(paste("Processing", length(file.names), "image files:"))
        prog.bar <- txtProgressBar(min = 0, max = length(file.names), style = 3)
      }

      for (file.ix in seq_along(file.names)) {

        file.name <- file.names[file.ix]

        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, file.ix)
        # browser()
        cfa <- split.cfa(file.name, file.path)
        rgb <- map.to.rgb$convert.to.rgb(cfa)

        mean.r <- channelMean(rgb$r)
        mean.g <- channelMean(rgb$g)
        mean.b <- channelMean(rgb$b)

        var.r <- channelVar(rgb$r)
        var.g <- channelVar(rgb$g)
        var.b <- channelVar(rgb$b)

        cov.rg <- channelCov(rgb$r, rgb$g)
        cov.rb <- channelCov(rgb$r, rgb$b)
        cov.gb <- channelCov(rgb$g, rgb$b)

        var.df <- data.table::rbindlist(list(
          var.df,
          data.frame(
            "pict" = file.name
            ,"channel" = factor(c(1L,2L,3L), levels=c(1L,2L,3L), labels=rgb.labels)
            ,"mean" = c(mean.r, mean.g, mean.b)
            ,"var" = c(var.r, var.g, var.b)
            ,row.names = NULL
          )
        ))

        cov.df <- data.table::rbindlist(list(
          cov.df,
          data.frame(
            "pict" = file.name
            ,"chan.a" = factor(c(1L,1L,2L), levels=c(1L,2L,3L), labels=rgb.labels)
            ,"chan.b" = factor(c(2L,3L,3L), levels=c(1L,2L,3L), labels=rgb.labels)
            ,"cov" = c(cov.rg, cov.rb, cov.gb)
            ,row.names = NULL
          )
        ))
      }

      # Save the results
      private$.var.df <- var.df
      private$.cov.df <- cov.df

      # Get and save the standard model
      get.model.src.data.func <- imgnoiser.option('get.model.src.data')
      model.src.data <- get.model.src.data.func('std.var', self)
      private$.std.src.data <- model.src.data

      if (!show.progress)
        message(paste(length(file.names), "image samples were successfully processed as RGBs."))
    }

    ##------------------------------
    ## fit.model (documented here)
    ##------------------------------
    ,fit.model = function(
       model.name = imgnoiser.option('fit.model.name')
      ,model.family = imgnoiser.option('fit.model.family')
      ,degree = 2L
      ,formula = NULL
      ,model.data.name = imgnoiser.option('fit.model.data')
      , ...
    ) {

      super$fit.model(self, model.name, model.family, degree, formula, model.data.name, ...)
      invisible(self);
    }

  )
)

#**********************
# Begin of Documentation
#**********************
vvm.doc <- list()

#----------------------------------------------
#' Process the image samples.
#'
#' Read image samples and compute variance and covariance statistics for
#' each channnel.
#'
#' The calling arguments describe the list of raw image files that will be
#' processed. The image samples file names are expected to have the following
#' pattern:
#'
#' \emph{\code{<img.file.name>}<index>\code{<img.file.name.ext>}}
#'
#' Where \code{<img.file.name>} and \code{<img.file.name.ext>} are arguments of
#' this function. An example of file names with this pattern is ('crop_1.fit',
#' 'crop_2.fit', ...), where 'crop_' is given in \code{<img.file.name>} and
#' 'fit' is given in \code{<img.file.name.ext>}.
#'
#' The file path to the files is given in the \code{file.path} argument. i.e. it
#' is must not be part of the \code{<img.file.name>} argument.
#'
#' The numbering iof the file names must start with 1 and end with
#' \code{img.file.count} value without any gap in in the sequence.
#'
#' If your sample file names do not follow this pattern, you can specify them as
#' a character vector used as argument for the \code{<img.file.name>} parameter.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$digest(
#'      img.file.name = stop("The 'img.file.name' argument is required"),
#'      img.file.count = NULL,
#'      file.path = './',
#'      img.file.name.ext = '.fit'
#'      )
#'  }
#'
#' @param img.file.name A single character value, with the prefix of the raw
#'   image samples file names. It can also be a character vector with the whole
#'   file name sequence of the image samples, but without the file path, which
#'   should be given in the \code{file.path} argument.
#'
#' @param img.file.count The number of image sample files. If the
#'   \code{<img.file.name>} argument contains the whole file name sequence this
#'   argument is irrelevant and consequently ignored.
#'
#' @param file.path The file path where the raw image file samples are located.
#'
#' @param img.file.name.ext The file name extension of the raw image file
#'   samples. It can be 'fit' or 'fits'. This file name extension may start with
#'   a period as in '.fit' or not (e.g. 'fit'). When required, the function will add
#'   the period '.' to build the sequence of file names.
#'
#'   The supported values for the \code{img.file.name.ext} are '.fit' or '.fits'
#'   for the \href{http://fits.gsfc.nasa.gov/}{'FITS'} format or '.pgm' for the
#'   \href{http://netpbm.sourceforge.net/doc/pgm.html}{'PGM'} format. The letter
#'   case is irrelevant for the file name extension.
#'
#' @return A \code{invisible} instance of the calling object.
#'
#' @seealso See the section \emph{"Input data for the *vvm* Procedure"} in the
#'   \emph{"Introduction to the *vvm* technique"} vignette,
#'   \code{\link{vvm}}.
#'
#' @examples
#' \dontrun{
#'
#' # Create a new vvm instance
#' my.vvm <- vvm$new(has.RGGB.pattern=TRUE)
#'
#' # Process 40 files names 'crop_1.fit', 'crop_2.fit'...
#' # in the ./data-raw/ folder
#' my.vvm$digest("crop_", 40, './data-raw/')
#' }
#'
#' @name vvm$digest
#----------------------------------------------
vvm.doc$digest <- function(
   img.file.name = stop("The 'img.file.name' argument is required")
  ,img.file.count = NULL
  ,file.path = './'
  ,img.file.name.ext = '.fit'
  )
  NULL

#----------------------------------------------
#' Get the resulting noise variance data.
#'
#' Returns the variance data computed by the \code{\link{vvm$digest}} function
#' from each pair of images samples taken under identical photographic
#' conditions.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$var.df
#'  }
#'
#' @return A data.frame with the variance data from the channels in the image
#'   samples processed by the \code{\link{vvm$digest}} function. It contains the
#'   following columns:
#'
#' \itemize{
#'
#'  \item \code{pict}: Name of the processed image sample file.
#'
#'  \item \code{channel}: The label of the channel in the sample file that was
#'  processed.
#'
#'  \item \code{mean}: The mean of the photosites values in the channel
#'  \code{channel} in the image sample \code{pict}.
#'
#'  \item \code{var}: The variance of the photosites values in the channel
#'  \code{channel} in the image sample \code{pict}.
#' }
#'
#' As a convenience, when possible, there is a channel labeled \emph{"Green
#' Avg"} with information summarizing both green channels in the sample. However
#' this label can be customized in the instance object creation with the
#' \code{\link{vvm$new}} function.
#'
#' When this data is included the \code{mean} column contains the mean
#' photosites values considering both green channels as a single whole, the same
#' applies for the corresponding \code{var} column.
#'
#' This data is computed only if the green channels have been specified in the
#' instance creation using the \code{green.channels} argument of the
#' \code{\link{vvm$new}} function.
#'
#' @examples
#' \dontrun{
#'
#' # print the head of the variance data frame
#' head(my.vvm$var.df)
#'
#' # Pass the variance data to other variable
#' var <- my.vvm$var.df
#'
#' # From here, it is just as like any data frame.
#' # You can, for example, get a subset with the red channel rows with mean above 2000
#' red.var <- subset(var, channel =='Red' & mean > 2000, var)
#'  }
#'
#' @name vvm$var.df
#----------------------------------------------
vvm.doc$var.df <- function() NULL


#----------------------------------------------
#' Get the resulting noise covariance data
#'
#' Returns the covariance data computed by the \code{\link{vvm$digest}} function
#' from each images sample.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$cov.df
#'  }
#'
#' @return A data.frame with the covariance data from the channels in the image
#'   samples computed by the \code{\link{vvm$digest}} function. It contains the
#'   following columns:
#'
#' \itemize{
#'
#'  \item \code{pict1}: Name of the processed image sample file.
#'
#'  \item \code{chan.a, chan.b}: The label of the channels whose covariance is shown
#'  in the \code{cov} column.
#'
#'  \item \code{cov}: The covariance between the channels \code{chan.a, chan.b} in the
#'  \code{pict1} image sample.
#' }
#'
#' @examples
#' \dontrun{
#'
#' # print the head of the variance data frame
#' head(my.vvm$cov.df)
#'
#' # Merge the variance and covariance data
#' merged <- merge(hvd$var.df, hvd$cov.df, by=c('pict1','pict2', 'cond'))
#' # Plot all the covariances (y axis) with respect to the means (x axis)
#' plot(merged$mean, merged$cov)
#' }
#'
#' @name vvm$cov.df
#----------------------------------------------
vvm.doc$cov.df <- function() NULL

rm(vvm.doc)

#**********************
# End of Documentation
#**********************
