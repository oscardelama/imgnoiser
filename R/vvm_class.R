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


#' @include noiseVar_class.R
#' @export
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

      known.greens <- (length(private$.green.channels) == 2)

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

        cfa <- split_cfa(file.name, file.path)

        # Build a sythetic channel with the average of both green channels
        if (known.greens) {
          chA <- array(0, dim = c(dim(cfa$ch4),2))
          chA[,,1] <- cfa[[private$.green.channels[1]]]
          chA[,,2] <- cfa[[private$.green.channels[2]]]
          cfa$chA <- apply(chA, c(1,2), mean)
        } else
          cfa$chA <- NA

        mean.ch1 <- channelMean(cfa$ch1)
        mean.ch2 <- channelMean(cfa$ch2)
        mean.ch3 <- channelMean(cfa$ch3)
        mean.ch4 <- channelMean(cfa$ch4)
        mean.chA <- channelMean(cfa$chA)

        var.ch1 <- channelVar(cfa$ch1)
        var.ch2 <- channelVar(cfa$ch2)
        var.ch3 <- channelVar(cfa$ch3)
        var.ch4 <- channelVar(cfa$ch4)
        var.chA <- channelVar(cfa$chA)*2

        cov.ch12 <- channelCov(cfa$ch1, cfa$ch2)
        cov.ch13 <- channelCov(cfa$ch1, cfa$ch3)
        cov.ch14 <- channelCov(cfa$ch1, cfa$ch4)
        cov.ch1A <- channelCov(cfa$ch1, cfa$chA)

        cov.ch23 <- channelCov(cfa$ch2, cfa$ch3)
        cov.ch24 <- channelCov(cfa$ch2, cfa$ch4)
        cov.ch2A <- channelCov(cfa$ch2, cfa$chA)

        cov.ch34 <- channelCov(cfa$ch3, cfa$ch4)
        cov.ch3A <- channelCov(cfa$ch3, cfa$chA)

        var.df <- data.table::rbindlist(list(
          var.df,
          data.frame(
             "pict" = file.name
            ,"channel" = factor(c(1,2,3,4,5), levels=c(1,2,3,4,5), labels=self$channel.labels)
            ,"mean" = c(mean.ch1, mean.ch2, mean.ch3, mean.ch4, mean.chA)
            ,"var" = c(var.ch1, var.ch2, var.ch3, var.ch4, var.chA)
            ,row.names = NULL
          )
        ))

        cov.df <- data.table::rbindlist(list(
          cov.df,
          data.frame(
             "pict" = file.name
            ,"chan.a" = factor(c(1,1,1,1,2,2,2,3,3), levels=c(1,2,3,4,5), labels=self$channel.labels)
            ,"chan.b" = factor(c(2,3,4,5,3,4,5,4,5), levels=c(1,2,3,4,5), labels=self$channel.labels)
            ,"cov" = c(cov.ch12, cov.ch13, cov.ch14, cov.ch1A,
                       cov.ch23, cov.ch24, cov.ch2A, cov.ch34, cov.ch3A)
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
        message(paste(length(file.names), "image samples were successfully processed."))

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
#' The calling arguments describe the sequence of raw image files that will be
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
#' @usage
#'   vvm.doc$digest(
#'      img.file.name = stop("The 'img.file.name' argument is required"),
#'      img.file.count = NULL,
#'      file.path = './',
#'      img.file.name.ext = '.fit'
#' )
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
#'   a period as in '.fit' or not ('fit'). When required, the function will add
#'   the period '.' to build the sequence of file names.
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
#' Get the resulting variance data.
#'
#' Returns the variance data computed by the \code{\link{vvm$digest}} function
#' from each pair of images samples taken under identical photographic
#' conditions.
#'
#' After the \emph{vvm} processing, the resulting variance and mean value
#' are expected to have a linear relationship.
#'
#' @usage
#'   vvm$get.var()
#'
#' @return A data.frame with the variance data from the channels in the
#' image samples processed by the \code{\link{digest}} function. It contains the following
#' columns:
#'
#' \itemize{
#'
#'  \item \code{cond}: A key for the photographic conditions common to the two
#'  processed samples. The detail of the photographic conditions is returned
#'  by the \code{\link{vvm$get.photo.conditions}} function, where the key is
#'  the value in this \code{cond} value.
#'
#'  \item \code{pict1, pict2}: Name of the processed image samples files.
#'
#'  \item \code{channel}: The label of the channel whose processed mean
#'  and variance are shown in the following columns.
#'
#'  \item \code{mean}: The mean of the photosites values of the average image,
#'  computed pixel by pixel, from the samples \code{pict1} and \code{pict2}.
#'
#'  \item \code{var}: The half of the photosite values variance of the
#'  difference image, computed pixel by pixel, of the \code{pict1} image minus
#'  the \code{pict2} one.
#' }
#'
#' @examples
#' # print the head of the variance data frame
#' head(my.vvm$get.var())
#'
#' # Pass the variance data to other variable
#' var <- my.vvm$get.var()
#'
#' # From here, it is just as like any data frame.
#' # You can, for example, get a subset with the red channel rows with mean above 2000
#' red.var <- subset(var, channel =='Red' & mean > 2000, var)
#'
#' @name vvm$get.var
#----------------------------------------------
vvm.doc$get.var <- function() NULL


#----------------------------------------------
#' Get the resulting covariance data.
#'
#' Returns the covariance data computed by the \code{\link{digest}} function
#' from each pair of images samples taken under identical photographic
#' conditions.
#'
#' @usage
#'   vvm$get.cov()
#'
#' @return A data.frame with the covariance data from the channels in the image
#'   samples computed by the \code{\link{digest}} function. It contains the
#'   following columns:
#'
#' \itemize{
#'
#'  \item \code{cond}: A key for the photographic conditions common to the two
#'  processed samples. The detail of the photographic conditions is returned
#'  by the \code{\link{vvm$get.photo.conditions}} function, where the key is
#'  the value in this \code{cond} value.
#'
#'  \item \code{pict1, pict2}: Name of the processed image samples files.
#'
#'  \item \code{chan.a, chan.b}: The label of the channels whose covariance is shown
#'  in the following column \code{cov}.
#'
#'  \item \code{cov}: The covariance between the channels \code{pict1, pict2} of the
#'  difference image, computed pixel by pixel, of the \code{pict1} image minus
#'  the \code{pict2} one.
#' }
#'
#' @examples
#' # print the head of the variance data frame
#' head(my.vvm$get.cov())
#'
#' # Merge the variance and covariance data
#' merged <- merge(hvd$get.var(), hvd$get.cov(), by=c('pict1','pict2', 'cond'))
#' # Plot all the covariances (y axis) with respect to the means (x axis)
#' plot(merged$mean, merged$cov)
#'
#' @name vvm$get.cov
#----------------------------------------------
vvm.doc$get.cov <- function() NULL

rm(vvm.doc)

#**********************
# End of Documentation
#**********************
