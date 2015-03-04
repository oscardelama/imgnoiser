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

    ,append.from = function(
      vvm.obj = stop("Missing 'vvm.obj' argument.")
    )
    {
      if ('vvm' %nin% class(vvm.obj))
        stop("The 'vvm.obj' argument must have the 'vvm' class.")

      # If this object is empty clone the other object here
      if (nrow(private$.var.df) == 0) {
        bag <- list()
        bag <- vvm.obj$.pack(bag)
        self$.unpack(bag)
      } else {
        private$.var.df <- data.table::rbindlist(list(
                              private$.var.df,
                              vvm.obj$var.df
                            ))

        private$.cov.df <- data.table::rbindlist(list(
                              private$.cov.df,
                              vvm.obj$cov.df
                            ))
        bag <- list()
        bag <- vvm.obj$.pack(bag)

        private$.std.src.data$data <- data.table::rbindlist(list(
                              private$.std.src.data$data,
                              bag[['noise.var.class']][['std.src.data']][['data']]
                            ))
      }
      # Make sure pict is not a factor
      private$.var.df$pict <- as.character(private$.var.df$pict)
      private$.cov.df$pict <- as.character(private$.cov.df$pict)
    }

    ##------------------------------
    ## load
    ##------------------------------
    ,digest = function(
        file.name.from  = stop("The 'file.name.from' argument is missing.")
        ,file.name.to   = stop("The 'file.name.to' argument is missing.")
        ,file.path  = './'
        ,file.name.ext  = '.pgm'
        ,min.raw        = 0
        ,max.raw        = Inf
    )
    {
      # file.names <- get.img.file.names(img.file.name, img.file.count, img.file.name.ext)
      file.path <- valid.file.path(file.path)
      file.names <- select.file.range(file.name.from, file.name.to, file.name.ext, file.path)

      # Validate min and max arguments
      if (!is.null(min.raw) && !is.null(max.raw)) {
        vector.alike(min.raw, c(1,4), type='n', all.unique=FALSE)
        vector.alike(max.raw, c(1,4), type='n', all.unique=FALSE)

        force.four.values <- function(v) {
          if (length(v) == 4) v
          else rep(v[1], 4)
        }
        min.raw <- force.four.values(min.raw)
        max.raw <- force.four.values(max.raw)
        valid.limits <- TRUE
      } else
        valid.limits <- FALSE

      # Placeholders for the resulting data
      cov.df <- data.frame()
      var.df <- data.frame()
      # Reset the variables depending on the result of this function
      private$.merged.var.cov.df <- NULL
      private$.var.df <- data.frame()
      private$.cov.df <- data.frame()
      private$.std.src.data <- list()
      private$.model <- list()
      sqrt2 <- sqrt(2)

      known.greens <- (!is.null(private$.green.channels) & length(private$.green.channels) == 2)

      # Check there is not clipping above 2%
      bad.pixel.count.limit <- 0
      channel.is.valid <- function(channels, channel.idx) {
        if (bad.pixel.count.limit == 0) bad.pixel.count.limit <<- 0.02 * length(channels[[channel.idx]])
        (out.of.range.pixel.count(
                          channels[[channel.idx]],
                          min.raw[channel.idx],
                          max.raw[channel.idx]
                          ) < bad.pixel.count.limit);
      }

      # Get show.progress option
      show.progress <- package.option('show.progress')
      # Show progress bar
      if (show.progress) {
        message(paste0("Processing ", length(file.names), " image files:"))
        prog.bar <- txtProgressBar(min = 1L, max = length(file.names), style = 3L)
      }

      for (file.ix in seq_along(file.names)) {

        file.name <- file.names[file.ix]

        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, file.ix)

        cfa <- split_channels(file.name, file.path)

        # browser()
        #-- Validate channel values are in [min, max] range
        if (valid.limits) {
          if (!channel.is.valid(cfa, 1L)) cfa$ch1 <- NA
          if (!channel.is.valid(cfa, 2L)) cfa$ch2 <- NA
          if (!channel.is.valid(cfa, 3L)) cfa$ch3 <- NA
          if (!channel.is.valid(cfa, 4L)) cfa$ch4 <- NA
        }
        #--
        # browser()
        valid.greens <- ( known.greens &&
                          is.channel(cfa[[private$.green.channels[1L]]]) &&
                          is.channel(cfa[[private$.green.channels[2L]]]) )

        # Build a synthetic channel with the average pixel value in both green channels
        if (valid.greens) {
          chA <- array(0, dim = c(dim(cfa[[private$.green.channels[1L]]]), 2L))
          chA[,,1L] <- cfa[[private$.green.channels[1L]]]
          chA[,,2L] <- cfa[[private$.green.channels[2L]]]
          cfa$chA <- apply(chA, c(1L,2L), mean)
          chA <- NULL
        }
        else
          cfa$chA <-  NA

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
        cov.ch1A <- channelCov(cfa$ch1, cfa$chA)*sqrt2

        cov.ch23 <- channelCov(cfa$ch2, cfa$ch3)
        cov.ch24 <- channelCov(cfa$ch2, cfa$ch4)
        cov.ch2A <- channelCov(cfa$ch2, cfa$chA)*sqrt2

        cov.ch34 <- channelCov(cfa$ch3, cfa$ch4)
        cov.ch3A <- channelCov(cfa$ch3, cfa$chA)*sqrt2

        cov.ch4A <- channelCov(cfa$ch4, cfa$chA)*sqrt2

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
      var.df <- var.df[!is.na(var.df$mean),]
      cov.df <- cov.df[!is.na(cov.df$cov),]
      # Make sure pict is not a factor
      var.df$pict <- as.character(var.df$pict)
      cov.df$pict <- as.character(cov.df$pict)

      # Save the results
      private$.var.df <- var.df
      private$.cov.df <- cov.df

      # Get and save the standard model
      get.model.src.data.func <- imgnoiser.option('get.model.src.data')
      model.src.data <- get.model.src.data.func('std.var', self)
      private$.std.src.data <- model.src.data

      if (!show.progress)
        msg(paste(length(file.names), "image samples were successfully processed."))
    }

    ,digest.as.lab = function(
      file.name.from  = stop("The 'file.name.from' argument is missing.")
      ,file.name.to   = stop("The 'file.name.to' argument is missing.")
      ,file.path      = './'
      ,file.name.ext  = 'pgm'
      ,min.raw        = 0
      ,max.raw        = Inf
      ,is.neutral     = FALSE
      ,map.to.lab     = stop("The 'map.to.lab' argument is missing.")
      ,lab.scale      = 100
      ,gamma          = 1/3
    )
    {
      lab.labels <- c('L', 'a', 'b')

      self$digest.as.rgb (
        file.name.from
        ,file.name.to
        ,file.path
        ,file.name.ext
        ,min.raw
        ,max.raw
        ,is.neutral
        ,map.to.lab
        ,lab.scale
        ,lab.labels
        ,FALSE
        ,'linear'
        ,XYZ.of.illuminant('D50')
        ,gamma
      )
    }

    ,digest.as.rgb = function(
      file.name.from  = stop("The 'file.name.from' argument is missing.")
      ,file.name.to   = file.name.from
      ,file.path      = './'
      ,file.name.ext  = 'pgm'
      ,min.raw        = 0
      ,max.raw        = Inf
      ,is.neutral     = FALSE
      ,map.to.rgb     = stop("The 'map.to.rgb' argument is missing.")
      ,rgb.scale      = 255
      ,rgb.labels     = imgnoiser.option('rgb.labels')
      ,use.camera.tc  = TRUE
      ,target.tc      = imgnoiser.option('tone.curve.id')
      # Parameters fro Lab conversion
      ,white.ref.XYZ  = XYZ.of.illuminant('D50')
      ,gamma          = 1/3
    )
    {

      # Validate RGGB.indices
      if (is.null(self$RGGB.indices))
        stop("The channels color are not known, set the 'RGGB.indices' argument at the object creation.")

      # Validate green channels
      if (is.null(private$.green.channels) | length(private$.green.channels) != 2)
        stop("The green channels indices are not known, set the 'green.channels' argument at the object creation.")

      # Validate rgb.scale
      if (map.to.rgb$target.space.id != 'Lab')
        vector.alike(rgb.scale, 1L, type='n', valid.range=c(1, 2L^16L))
      else
        vector.alike(rgb.scale, c(1L, 3L), type='n', valid.range=c(1, 2L^16L))

      # Validate the RGB labels
      vector.alike(rgb.labels, 3L)

      file.path <- valid.file.path(file.path)
      # Validate and get file names of samples
      file.names <- select.file.range(file.name.from, file.name.to, file.name.ext, file.path)

      # Validate min and max arguments
      if (!is.null(min.raw) && !is.null(max.raw)) {
        vector.alike(min.raw, c(1,4), type='n', all.unique=FALSE)
        vector.alike(max.raw, c(1,4), type='n', all.unique=FALSE)

        force.four.values <- function(v) {
          if (length(v) == 4) v
          else rep(v[1], 4)
        }
        min.raw <- force.four.values(min.raw)
        max.raw <- force.four.values(max.raw)
        valid.limits <- TRUE
      } else
        valid.limits <- FALSE

      # Placeholders for the resulting data
      cov.df <- data.frame()
      var.df <- data.frame()
      # Reset the variables depending on the result of this function
      private$.merged.var.cov.df <- NULL
      private$.var.df <- data.frame()
      private$.cov.df <- data.frame()
      private$.std.src.data <- list()
      private$.model <- list()

      # Initialize the RGB conversion (there are some additional validations)
      map.to.rgb$prepare.to.dest.conversions(rgb.scale, self$RGGB.indices,
                                             use.camera.tc, target.tc,
                                             white.ref.XYZ, gamma)

      # Get show.progress option
      show.progress <- (package.option('show.progress') == TRUE) && (length(file.names) > 1)
      # Show progress bar
      if (show.progress) {
        message(paste("Processing", length(file.names), "image files:"))
        prog.bar <- txtProgressBar(min = 1L, max = length(file.names), style = 3L)
      }

      # Check there is not clipping above 2%
      bad.pixel.count.limit <- 0
      channel.is.valid <- function(channels, channel.idx) {
        if (bad.pixel.count.limit == 0) bad.pixel.count.limit <<- 0.02 * length(channels[[channel.idx]])
        (out.of.range.pixel.count(
          channels[[channel.idx]],
          min.raw[channel.idx],
          max.raw[channel.idx]
        ) < bad.pixel.count.limit);
      }

      for (file.ix in seq_along(file.names)) {

        file.name <- file.names[file.ix]

        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, file.ix)
        cfa <- split_channels(file.name, file.path)

        #-- Validate all channel values are in range
        if (valid.limits == TRUE) {
          if (!channel.is.valid(cfa, 1L) ||
              !channel.is.valid(cfa, 2L) ||
              !channel.is.valid(cfa, 3L) ||
              !channel.is.valid(cfa, 4L)) next
        }
        #--

        rgb <- map.to.rgb$convert.raw.to.dest(cfa, is.neutral)

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
        msg(paste(length(file.names), "image samples were successfully processed as RGBs."))
    }

    ,digest.from.rgb = function(
      file.name.from  = stop("The 'file.name.from' argument is missing.")
      ,file.name.to   = stop("The 'file.name.to' argument is missing.")
      ,file.path      = './'
      ,file.name.ext  = '.tif'
      ,rgb.labels     = imgnoiser.option('rgb.labels')
      ,file.scale     = 1
      ,rgb.scale      = 255
    )
    {

      # Validate the RGB labels
      vector.alike(rgb.labels, 3L)
      # Validate and complete the path (will always end with backslash)
      file.path <- valid.file.path(file.path)
      # Validate and get file names of samples
      file.names <- select.file.range(file.name.from, file.name.to, file.name.ext, file.path)

      # Placeholders for the resulting data
      cov.df <- data.frame()
      var.df <- data.frame()
      # Reset the variables depending on the result of this function
      private$.merged.var.cov.df <- NULL
      private$.var.df <- data.frame()
      private$.cov.df <- data.frame()
      private$.std.src.data <- list()
      private$.model <- list()

      # Get show.progress option
      show.progress <- package.option('show.progress')
      # Show progress bar
      if (show.progress) {
        message(paste("Processing", length(file.names), "image files:"))
        prog.bar <- txtProgressBar(min = 1L, max = length(file.names), style = 3L)
      }

      for (file.ix in seq_along(file.names)) {

        file.name <- file.names[file.ix]

        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, file.ix)
        # browser()
        rgb <- split_rgb_channels(file.name, file.path, rgb.scale/file.scale)

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
        msg(paste(length(file.names), "image samples were successfully processed as RGBs."))
    }

    ##------------------------------
    ## fit.model (documented here)
    ##------------------------------
    ,fit.model = function(
       model.name = imgnoiser.option('fit.model.name')
      ,model.family = imgnoiser.option('fit.model.family')
      ,degree = 2L
      ,formula = NULL
      ,conf.level = imgnoiser.option('conf.level')
      ,model.data.name = imgnoiser.option('fit.model.data')
      , ...
    ) {

      super$fit.model(self, model.name, model.family, degree, formula, conf.level, model.data.name, ...)
      invisible(self);
    }

  )
)

#**********************
# Begin of Documentation
#**********************
vvm.doc <- list()

#----------------------------------------------
#' Process the raw image samples.
#'
#' Read raw image samples and compute variance and covariance statistics for
#' each channnel.
#'
#' The calling arguments describe the list of raw image files that will be
#' processed. The image samples file names are expected to have the following
#' pattern:
#'
#' \emph{\code{file.path}/\code{<img.file.base>}.\code{<file.name.ext>}}
#'
#' Where \code{<img.file.base>} is alfabetically between the
#' \code{file.name.from} and \code{file.name.to} arguments.
#'
#' In other words, the folder given in \code{file.path} is scanned looking
#' for files with base name between (and including) \code{file.name.from} and
#' \code{file.name.to}, having all of them the extension \code{file.name.ext}.
#'
#' If your sample file names do not follow this pattern, you can specify them as
#' a character vector as argument for the \code{<file.name.from>} parameter.
#'
#' The \code{\link{select.file.range}} may help you to get a baseline selection with
#' the files you want to get processed by this function.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$digest(
#'      file.name.from = stop("The 'file.name.from' argument is missing."),
#'      file.name.to   = stop("The 'file.name.to' argument is missing."),
#'      file.path      = './',
#'      file.name.ext  = '.pgm'
#'      min.raw        = 0
#'      max.raw        = Inf
#'      )
#'  }
#'
#' @param file.name.from
#' @param file.name.to The \code{file.name.from} and \code{file.name.to} are the
#'   alphabetical range of the desired files from the folder given in
#'   \code{file.path}. These names should not include the file name
#'   extension, which is specified with \code{file.name.ext}.
#'
#' @param file.path The file path where the raw image file samples are located.
#'
#' @param file.name.ext The file name extension of the raw image file samples.
#'   It can be 'fit', 'fits' or 'pgm'. This file name extension may start or not
#'   with a period as in '.fit' or fit'.
#'
#'   The supported values for the \code{img.file.name.ext} are '.fit' or '.fits'
#'   for the \href{http://fits.gsfc.nasa.gov/}{'FITS'} format or '.pgm' for the
#'   \href{http://netpbm.sourceforge.net/doc/pgm.html}{'PGM'} format.
#'
#' @param min.raw
#' @param max.raw These are the the expected pixel values range. If more than
#'   the 2% of pixel values in an image channel sample is outside this range,
#'   the channel is discarded.
#'
#'   Each parameter can be a single value or a vector of four values. For the
#'   single value case, the same value is used as limit for the four channels,
#'   otherwise there must be four values, one per channel and in the image sample
#'   channels order.
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
#' img.path <- 'D:/Noise-Study/Nikon-D7000/ISO-100'
#' my.vvm$digest('_ODL5695', '_ODL5767', '.pgm', img.path)
#' }
#'
#' @name vvm$digest
#----------------------------------------------
vvm.doc$digest <- function() NULL

#----------------------------------------------
#' Process the raw image samples after their conversion to RGB.
#'
#' Read raw image samples, convert them to a RGB color space and compute from
#' them noise variance and covariance channel statistics.
#'
#' The calling arguments describe the list of raw image files that will be
#' processed in the same way as in the \code{digest} function. The image samples
#' file names are expected to have the following pattern:
#'
#' \emph{\code{file.path}/\code{<img.file.base>}.\code{<file.name.ext>}}
#'
#' Where \code{<img.file.base>} is alfabetically between the
#' \code{file.name.from} and \code{file.name.to} arguments.
#'
#' In other words, the folder given in \code{file.path} is scanned looking
#' for files with base name between (and including) \code{file.name.from} and
#' \code{file.name.to}, having all of them the extension \code{file.name.ext}.
#'
#' If your sample file names do not follow this pattern, you can specify them as
#' a character vector as argument for the \code{<file.name.from>} parameter.
#'
#' The \code{\link{select.file.range}} may help you to get a baseline selection with
#' the files you want to get processed by this function.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$digest.as.rgb(
#'      file.name.from = stop("The 'file.name.from' argument is missing."),
#'      file.name.to   = stop("The 'file.name.to' argument is missing."),
#'      file.path      = './',
#'      file.name.ext  = '.pgm',
#'      min.raw        = 0,
#'      max.raw        = Inf,
#'      is.neutral     = FALSE,
#'      map.to.rgb     = stop("The 'map.to.rgb' argument is missing."),
#'      rgb.scale      = 255,
#'      rgb.labels     = imgnoiser.option('rgb.labels'),
#'      tone.curve     = imgnoiser.option('tone.curve.id')
#'      )
#'  }
#'
#' @param file.name.from
#' @param file.name.to The \code{file.name.from} and \code{file.name.to} are the
#'   alphabetical range of the desired files from the folder given in
#'   \code{file.path}. These names should not include the file name
#'   extension, which is specified with \code{file.name.ext}.
#'
#' @param file.path The file path where the raw image file samples are located.
#'
#' @param file.name.ext The file name extension of the raw image file samples.
#'   It can be 'fit', 'fits' or 'pgm'. This file name extension may start or not
#'   with a period as in '.fit' or fit'.
#'
#'   The supported values for the \code{img.file.name.ext} are '.fit' or '.fits'
#'   for the \href{http://fits.gsfc.nasa.gov/}{'FITS'} format or '.pgm' for the
#'   \href{http://netpbm.sourceforge.net/doc/pgm.html}{'PGM'} format.
#'
#' @param min.raw
#' @param max.raw These are the the expected pixel values range. If more than
#'   the 2% of pixel values in an image channel sample is outside this range,
#'   the whole image is discarded. This is because the RGB processing requires
#'   all the channels be valid.
#'
#'   Each parameter can be a single value or a vector of four values. For the
#'   former case, the same value is used as limit for the four channels,
#'   otherwise there must be four values, one per channel and in the image sample
#'   channels order.
#'
#' @param is.neutral A logical value indicating if the target of the samples is
#'   a neutral surface. When that is the case additional raw white-balance is
#'   done per in a per sample basis.
#'
#' @param map.to.rgb An object of the class \code{colmap} initialized with color
#'   data of the camera sensor whose image samples will be processed. This
#'   object must have selected a RGB color space target and white-balance
#'   calibrated for the samples that are going to be processed through the use
#'   of its function \code{colmap$get.conv.matrix.from.raw()}.
#'
#' @param rgb.scale The scale of the RGB pixel values after the transformation
#'   from the camera raw space. The pixel values will be between 0 and the
#'   \code{rgb.scale}.
#'
#'   In the conversion the RGB pixel values will be floating point numbers, not
#'   integers.
#'
#' @param rgb.labels A character vector with the three labels for the RGB
#'   colors. By default they are \code{c('red', 'green', 'blue')}.
#'
#' @param tone.curve A tone curve to be applied to the linear RGB colors after
#'   the conversion from linear raw to linear RGB. Possible character values
#'   are:
#'
#' \itemize{
#'
#'  \item \code{'camera.coldata'}: For the use of the tone curve in the data used
#'  to initialize the colmap object given in the \code{map.to.rgb} argument.
#'
#'  \item \code{'linear'}: To keep convert the raw values to the target RGB
#'  space but leaving the values in linear way.
#'
#'  \item \code{'sRGB'}: To use the tone curve prescribed by the sRGB color
#'  space.
#'
#'  \item \code{'BT.709'}: To use the tone curve prescribed by that standard.
#'
#'  \item \code{'Gamma.2.2'}: To use a standard 2.2 gamma tone curve.
#'
#'  \item \code{'Gamma.1.8'}: To use a standard 1.8 gamma tone curve.
#'
#' }
#'
#'   This argument can also be a tone curve by itself. In such case, its first
#'   two columns must numeric and it must have at least 8 rows, with values in
#'   the [0,1]range. The first columns will represent the source color values
#'   which will be mapped to the corresponding value in the second column.
#'
#'   During the tone curve application the missing values will be interpolated
#'   using smooth splines, with the required degree to pass over each given
#'   point.
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
#' # Create a colmap object
#' cm.obj <- imgnoiser::colmap$new(imgnoiser::nikon.d7000.ISO100.colmap)
#'
#' # Set target space to 'sRGB' and white-balance for the raw images
#' # neutral reference
#' neutral.raw <- c(3696, 7539, 5588)
#' cm.obj$get.conv.matrix.from.raw(neutral.raw, to.space = 'sRGB')
#'
#' # Create a new vvm instance
#' rgb.vvm <- vvm$new(has.RGGB.pattern = TRUE)
#' img.path <- 'H:/Noise-Study/Nikon-D700/ISO-100/'
#'
#' # Process the images
#' rgb.vvm$digest.as.rgb( file.name.from = '_ODL5695',
#'                        file.name.to   = '_ODL5767',
#'                        file.name.ext  = '.pgm',
#'                        file.path   = img.path,
#'                        is.neutral  = TRUE,
#'                        map.to.rgb  = cm.obj,
#'                        rgb.scale   = 255,
#'                        tone.curve  = 'camera.coldata')
#' #> 70 image samples were successfully processed as RGBs.
#' }
#'
#' @name vvm$digest.as.rgb
#----------------------------------------------
vvm.doc$digest.as.rgb <- function() NULL

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
