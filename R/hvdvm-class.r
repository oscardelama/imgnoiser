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
hvdvm <- R6::R6Class('hvdvm', inherit = noise.var,

  private = list(
    '.photo.conds.df' = data.frame()
  ),

  active = list(

    ##------------------------------
    ## photo.conditions.df  (documented here)
    ##------------------------------
    photo.conditions.df = function() {
      if (nrow(private$.photo.conds.df) == 0)
        warning('There is no ".photo.conds.df" information. You should probably run the digest() function before.')

      return(private$.photo.conds.df)
    }

    ,merged.var.cov.df = function() {

      if (is.null(private$.merged.var.cov.df))
        msg('This data is built at the first request and may take a few seconds, please be patient.')
      else
        return(private$.merged.var.cov.df)

      var.df <- as.data.frame(private$.var.df)
      cov.df <- as.data.frame(private$.cov.df)
      cov.df$mean.a <- 0
      cov.df$mean.b <- 0
      cov.df$var.a <- 0
      cov.df$var.b <- 0

      chan.comb <- dplyr::distinct(cov.df, chan.a, chan.b)[,c('chan.a', 'chan.b')]
      chan.comb$level <- 1:nrow(chan.comb)
      chan.comb$label <- paste0(chan.comb$chan.a, ", ", chan.comb$chan.b)
      cov.df$channel.combo <- factor(1, levels=chan.comb$level, labels=chan.comb$label)

      # Show progress bar
      show.progress <- package.option('show.progress')
      if (show.progress == TRUE) {
        message(paste("Processing", nrow(cov.df), "rows:"))
        prog.bar <- txtProgressBar(min = 1L, max = nrow(cov.df), style = 3L)
      }

      for (ix in 1:nrow(cov.df)) {
        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, ix)

        pict.1 <- cov.df[ix, 'pict1']
        pict.2 <- cov.df[ix, 'pict2']
        cov.chan.a <- cov.df[ix, 'chan.a']
        cov.chan.b <- cov.df[ix, 'chan.b']
        cov.df[ix, 'mean.a'] <- subset(var.df, pict1 == pict.1 & pict2 == pict.2 & channel == cov.chan.a, mean)
        cov.df[ix, 'mean.b'] <- subset(var.df, pict1 == pict.1 & pict2 == pict.2 & channel == cov.chan.b, mean)
        cov.df[ix, 'var.a']  <- subset(var.df, pict1 == pict.1 & pict2 == pict.2 & channel == cov.chan.a, var)
        cov.df[ix, 'var.b']  <- subset(var.df, pict1 == pict.1 & pict2 == pict.2 & channel == cov.chan.b, var)
        cov.df[ix, 'channel.combo']  <- subset(chan.comb, chan.a == cov.chan.a & chan.b == cov.chan.b, label)
      }
      private$.merged.var.cov.df <- cov.df
      var.df <- NULL
      cov.df <- NULL
      # Result
      private$.merged.var.cov.df;
    }

    ,wide.var.df = function() {
      var.df <- private$.var.df
      melted.df <- reshape2::melt(var.df, id=c('cond', 'pict1', 'pict2', 'channel'))
      wide.df <- reshape2::dcast(melted.df, cond + pict1 + pict2 ~ channel + variable)
      # Standardize the names
      wide.df.names <- tolower(names(wide.df))
      wide.df.names <- sub("_", ".", wide.df.names)
      wide.df.names <- make.names(wide.df.names, unique=TRUE)
      data.table::setnames(wide.df, wide.df.names)
      # result
      wide.df;
    }

  ),

  public = list(

    ##------------------------------
    ## pack  (documented here)
    ##------------------------------
    .pack = function(bag) {

      bag <- super$.pack(bag)
      bag[['hvdvm.class']] <- list(
        'photo.conds.df' = private$.photo.conds.df
      )
      bag;
    }

    ##------------------------------
    ## unpack  (documented here)
    ##------------------------------
    ,.unpack = function(bag) {

      my.bag <- bag[['hvdvm.class']]
      private$.photo.conds.df <- my.bag[['photo.conds.df']]
      bag[['hvdvm.class']] <- NULL

      super$.unpack(bag)
    }

    ##------------------------------
    ## digest  (documented here)
    ##------------------------------
    ,digest = function(
         photo.conds.file = stop("The 'photo.conds.file' argument is required.")
        ,crop.files.path = './'
        ,min.raw        = 0
        ,max.raw        = Inf
    ) {

      # browser()
      # Check photo.conds.file is the name of an existing file
      if (is.atomic(photo.conds.file)) {
        photo.conds.file <- valid.metadata.fname(photo.conds.file)
        # Read the metadata file
        meta.df <- read.csv(photo.conds.file, stringsAsFactors=FALSE)
      } else {
        if ('data.frame' %nin% class(photo.conds.file))
          stop('The photo.conds.file argument is neither a data frame nor a csv file name.')
        else
          meta.df <- photo.conds.file
      }

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

      # Reset the variables depending on the result of this function
      private$.merged.var.cov.df <- NULL
      private$.var.df <- data.frame()
      private$.cov.df <- data.frame()
      private$.std.src.data <- list()
      private$.model <- list()

      # Map the column names in the photo.conds.file
      meta.df <- map.col.names(meta.df, imgnoiser.option('conds.col.map'))

      crop.files.path <- valid.file.path(crop.files.path)

      # Get show.progress option
      show.progress <- package.option('show.progress')

      #-- Memoize (locally) channel splitting
      splitted.channels <- list()
      split.channels <- function(.file.name, crop.files.path) {
        if (length(splitted.channels) == 0 || .file.name %nin% names(splitted.channels)) {
          channels <- split_channels(.file.name, crop.files.path)
          splitted.channels[[.file.name]] <<- channels
          return (channels)
        } else
          return (splitted.channels[[.file.name]])
      }
      #---

      # Pre-Subsetting for performance reasons
      channel.labels <- private$.channel.labels[1:4]

      # Find the pictures taken with the same conditions
      tbl <- dplyr::tbl_df(meta.df)
      grb <- dplyr::group_by(tbl, lighting, iso, shutter.speed, aperture, focal.length)
      sel <- dplyr::select(grb, lighting, iso, shutter.speed, aperture, focal.length)
      sum <- dplyr::summarise(sel, count = n())
      photo.conds.df <- dplyr::filter(sum, count > 1L)
      photo.conds.df <- cbind(cond = 1L:nrow(photo.conds.df), photo.conds.df)
      # head(photo.conds.df)

      #Get the total number of combinations we will process
      total.combin.count <- sum(choose(photo.conds.df$count, 2L))

      # Placeholders for the resulting data
      hvdvm.df <- data.frame()
      half.cov.df <- data.frame()

      if (show.progress) {
        writeLines(paste("Processing", total.combin.count, "different combinations:"))
        prog.bar <- txtProgressBar(min = 1L, total.combin.count, style = 3L)
        combin.counter <- 0
      }

      # Loop the photo.conds.df data frame
      for(ix.cond in 1L:nrow(photo.conds.df)) {

        # Read the condition
        cond <- photo.conds.df[ix.cond,]

        # Find the pics taken with this condition
        picts <- subset(meta.df,
                        lighting == cond$lighting &
                        iso == cond$iso &
                        shutter.speed == cond$shutter.speed &
                        aperture == cond$aperture &
                        focal.length == cond$focal.length
        )

        # Find the combinations of two pictures for the
        # pictures taken with these common conditions
        combin <- data.frame(t(combn(1L:cond$count, 2L)))
        colnames(combin) <- c("pic1", "pic2")

        # Placeholders for the pictures data
        pict1 <- list()
        pict2 <- list()

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

        #@TODO: enhance this ugly code
        delta.ch <- NA
        ch.mean <- NA
        get.channel.delta.and.mean <- function(pic1, pic2, idx) {
          # Validate if channel values are in the desired range
          if ((valid.limits == TRUE) && (!channel.is.valid(pic1, idx) || !channel.is.valid(pic2, idx))) {
            delta.ch <<- NA
            ch.mean <<- NA
          } else {
            delta.ch <<- c(pic1[[idx]] - pic2[[idx]])
            ch.mean <<- mean(c(mean(pic1[[idx]]), mean(pic2[[idx]])))
          }
        }

        # For each pair of pictures combination
        for(ix.comb in 1L:nrow(combin)) {

          # Show progress
          if (show.progress) {
            combin.counter <- combin.counter + 1L
            setTxtProgressBar(prog.bar, combin.counter)
          }

          # Read the pictures and split them into channels
          pict1 <- split.channels(picts[combin[ix.comb, "pic1"], "crop.file.name"], crop.files.path)
          pict2 <- split.channels(picts[combin[ix.comb, "pic2"], "crop.file.name"], crop.files.path)

          # if (picts[combin[ix.comb, "pic1"], "crop.file.name"] == '_ODL1280.pgm') browser()
          # browser()

          # Get delta channel and channel mean
          get.channel.delta.and.mean(pict1, pict2, 1L)
          delta.ch1 <- delta.ch
          mean.ch1 <- ch.mean

          get.channel.delta.and.mean(pict1, pict2, 2L)
          delta.ch2 <- delta.ch
          mean.ch2 <- ch.mean

          get.channel.delta.and.mean(pict1, pict2, 3L)
          delta.ch3 <- delta.ch
          mean.ch3 <- ch.mean

          get.channel.delta.and.mean(pict1, pict2, 4L)
          delta.ch4 <- delta.ch
          mean.ch4 <- ch.mean

          # Compute the half variance of delta channels
          halfVar.delta.ch1 <- channelVar(delta.ch1)/2
          halfVar.delta.ch2 <- channelVar(delta.ch2)/2
          halfVar.delta.ch3 <- channelVar(delta.ch3)/2
          halfVar.delta.ch4 <- channelVar(delta.ch4)/2

          # Compute half covariances of delta channels
          halfCov.ch12 <- channelCov(delta.ch1, delta.ch2)/2
          halfCov.ch13 <- channelCov(delta.ch1, delta.ch3)/2
          halfCov.ch14 <- channelCov(delta.ch1, delta.ch4)/2
          halfCov.ch23 <- channelCov(delta.ch2, delta.ch3)/2
          halfCov.ch24 <- channelCov(delta.ch2, delta.ch4)/2
          halfCov.ch34 <- channelCov(delta.ch3, delta.ch4)/2

#           # Get the mean of each channels averaging the same channel
#           # on each picture
#           mean.ch1 <- channelMean(pict1$ch1 + pict2$ch1)/2
#           mean.ch2 <- channelMean(pict1$ch2 + pict2$ch2)/2
#           mean.ch3 <- channelMean(pict1$ch3 + pict2$ch3)/2
#           mean.ch4 <- channelMean(pict1$ch4 + pict2$ch4)/2

          # Save half variance versus mean
          hvdvm.df <- data.table::rbindlist(list(
                            hvdvm.df,
                            data.frame(
                               "cond" = ix.cond
                              ,"pict1" = picts[combin[ix.comb, "pic1"], "crop.file.name"]
                              ,"pict2" = picts[combin[ix.comb, "pic2"], "crop.file.name"]
                              ,"channel" = factor(c(1L,2L,3L,4L), levels=c(1L,2L,3L,4L), labels=channel.labels)
                              ,"mean" = c(mean.ch1, mean.ch2, mean.ch3, mean.ch4)
                              ,"var" = c(halfVar.delta.ch1, halfVar.delta.ch2, halfVar.delta.ch3, halfVar.delta.ch4)
                              ,row.names = NULL
                          )
                      ))

          # Save half variance versus mean
          half.cov.df <- data.table::rbindlist(list(
                              half.cov.df,
                              data.frame(
                                 "cond" = ix.cond
                                ,"pict1" = picts[combin[ix.comb, "pic1"], "crop.file.name"]
                                ,"pict2" = picts[combin[ix.comb, "pic2"], "crop.file.name"]
                                ,"chan.a" = factor(c(1L,1L,1L,2L,2L,3L), levels=c(1L,2L,3L,4L), labels=channel.labels)
                                ,"chan.b" = factor(c(2L,3L,4L,3L,4L,4L), levels=c(1L,2L,3L,4L), labels=channel.labels)
                                ,"cov.a.b" = c(halfCov.ch12, halfCov.ch13, halfCov.ch14,
                                           halfCov.ch23, halfCov.ch24, halfCov.ch34)
                              )
                          ))
        }
      }

      # Save the returned values in the object
      private$.var.df <- dplyr::tbl_df(hvdvm.df)
      private$.cov.df <- dplyr::tbl_df(half.cov.df)
      private$.photo.conds.df <- dplyr::tbl_df(photo.conds.df)

      # Get and save the standard model
      get.model.src.data.func <- imgnoiser.option('get.model.src.data')
      model.src.data <- get.model.src.data.func('std.var', self)
      private$.std.src.data <- model.src.data

      if (!show.progress)
        msg(paste(total.combin.count, "different sample combinations were successfully processed."))

      invisible(self)
    }

    ##------------------------------
    ## fit.model (documented here)
    ##------------------------------
    ,fit.model = function(
       model.name = imgnoiser.option('fit.model.name')
      ,model.family = imgnoiser.option('fit.model.family')
      ,degree = 1L
      ,formula = NULL
      ,conf.level = imgnoiser.option('conf.level')
      ,model.data.name = imgnoiser.option('fit.model.data')
      , ...
    ) {

      super$fit.model(self, model.name, model.family, degree, formula, conf.level, model.data.name, ...)
      invisible(self)
    }

  )
)

#**********************
# Begin of Documentation
#**********************
hvdvm.doc <- list()

#----------------------------------------------
#' Process the image samples.
#'
#' Process the image samples with the \emph{hvdvm} technique
#'
#' You can find more details about the input for this process in the
#' \emph{"Introduction to the *hvdvm* technique"} vignette.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$digest(
#'      photo.conds.file,
#'      crop.files.path = './',
#'      min.raw        = 0,
#'      max.raw        = NULL
#'      )
#'  }
#'
#' @param photo.conds.file The name of a .csv file describing the photographic
#'   conditions under which each photograph was taken to produce the samples.
#'   Also, this argument can already be a \code{data.frame} with those photographic
#'   conditions.
#'
#'   The following list is the set of columns expected in this data. It can
#'   contain more columns, but they will be ignored.
#'
#'   \itemize{
#'     \item\code{crop.file.name}
#'     \item\code{lighting}
#'     \item\code{iso}
#'     \item\code{shutter.speed}
#'     \item\code{aperture}
#'     \item\code{focal.length}
#'   }
#'
#'   If your data contains these information but using another column names, you
#'   can map your column names to the expected ones using the option
#'   \code{\link{imgnoiser.option}('conds.col.map')}.
#'
#'   For example, if you have a column named \code{ShutterSpeed} which is not
#'   the expected \code{shutter.speed} column name, the \code{conds.col.map}
#'   option must contain an entry with \code{ShutterSpeed} as key and
#'   \code{shutter.speed} as value, as in \code{list('ShutterSpeed' =
#'   'shutter.speed')}.
#'
#'   The image sample file names in the \code{crop.file.name} column may have the
#'   '.fit' or '.fits' extension for the
#'   \href{http://fits.gsfc.nasa.gov/}{'FITS'} format or '.pgm' for the
#'   \href{http://netpbm.sourceforge.net/doc/pgm.html}{'PGM'} format. The letter
#'   case is irrelevant for the file name extension.
#'
#' @param crop.files.path The \code{photo.conds.file} argument refers to a file
#'   containing a column named \code{crop.file.name} with the name of each image
#'   sample file. Those files are expected to be in one same folder. This
#'   argument must contain the path to that folder, which should not be part of
#'   the \code{crop.file.name} values.
#'
#' @param min.raw
#' @param max.raw These are the the expected pixel values range. If one or more
#'   pixel values in an image channel sample is outside this range, the whole
#'   image is discarded. This is because the RGB processing requires all image
#'   channels.
#'
#'   Both \code{min.raw} and \code{max.raw} must be supplied, otherwise they
#'   are ignored.
#'
#'   Each parameter can be a single value or a vector of four values. For the
#'   former case, the same value is used as limit for the four channels,
#'   otherwise there must be four values, one per channel and in the image sample
#'   channels order.
#'
#' @return The data resulting from this process can be seen using the functions
#'   \code{\link{hvdvm$var.df}}, \code{\link{hvdvm$cov.df}} and
#'   \code{\link{hvdvm$photo.conditions.df}}. Also, you can plot the result
#'   using the \code{\link{hvdvm$plot}} function.
#'
#' @seealso See the section \emph{"Input data for the *hvdvm* Procedure"} in the
#'   \emph{"Introduction to the *hvdvm* technique"} vignette,
#'   \code{\link{hvdvm}}.
#'
#' @examples
#' \dontrun{
#'
#' # In this example, the conditions file and the image sample files are in
#' # different folders, in this case specified with respect to the
#' # current working directory.
#' my.hvdvm$digest('./ISO100/White.csv', './ISO100/samples/')
#' }
#'
#' @name hvdvm$digest
#----------------------------------------------
hvdvm.doc$digest <- function() NULL


#----------------------------------------------
#' Get the resulting variance data.
#'
#' Returns the variance data computed by the \code{\link{hvdvm$digest}}
#' function.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$var.df
#'  }
#'
#' @return A data.frame with the variance data from the channels in the image
#'   samples processed by the \code{\link{hvdvm$digest}} function. It contains the
#'   following columns:
#'
#' \itemize{
#'
#'  \item \code{cond}: A key for the photographic conditions common to the two
#'  processed samples. The detail of the photographic conditions is returned
#'  by the \code{\link{hvdvm$photo.conditions.df}} function, where the key is
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
#' \dontrun{
#'
#' # print the head of the variance data frame
#' head(my.hvdvm$var.df)
#'
#' # Pass the variance data to other variable
#' var <- my.hvdvm$var.df
#'
#' # From here, it is just as like any data frame.
#' # You can, for example, get a subset with the red channel rows with mean above 2000
#' red.var <- subset(var, channel =='Red' & mean > 2000, var)
#' }
#'
#' @seealso \code{\link{hvdvm$cov.df}}, \code{\link{hvdvm$photo.conditions.df}}
#' @name hvdvm$var.df
#----------------------------------------------
hvdvm.doc$var.df <- function() NULL


#----------------------------------------------
#' Get the resulting covariance data.
#'
#' Returns the covariance data computed by the \code{\link{hvdvm$digest}}
#' function. This values are half of the covariance in the delta image, computed
#' for each pair of channels combination.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$cov.df
#'  }
#'
#' @return A data.frame with the covariance data from the channels in each delta
#'   image computed by the \code{\link{hvdvm$digest}} function. This data frame
#'   contains the following columns:
#'
#' \itemize{
#'
#'  \item \code{cond}: A key for the photographic conditions common to the two
#'  processed samples (\code{pict1, pict2}). The detail of the photographic
#'  conditions is returned by the \code{\link{hvdvm$photo.conditions.df}}
#'  function, where the key is the value in this \code{cond} value.
#'
#'  \item \code{pict1, pict2}: Name of the processed image samples files.
#'
#'  \item \code{chan.a, chan.b}: The label of the channels whose covariance is shown
#'  in the \code{cov} column.
#'
#'  \item \code{cov.a.b}: The half of the photosite values covariance in the
#'  \code{chan.a} and \code{chan.b} channels in the difference image, computed
#'  pixel by pixel, of the \code{pict1} image minus the \code{pict2} one.
#' }
#'
#' @examples
#' \dontrun{
#'
#' # print the head of the variance data frame
#' head(my.hvdvm$cov.df)
#'
#' # Merge the variance and covariance data
#' merged <- merge(hvd$var.df, hvd$cov.df, by=c('pict1','pict2', 'cond'))
#' # Plot all the covariances (y axis) with respect to the means (x axis)
#' plot(merged$mean, merged$cov)
#' }
#' @seealso \code{\link{hvdvm$var.df}}, \code{\link{hvdvm$photo.conditions.df}}
#' @name hvdvm$cov.df
#----------------------------------------------
hvdvm.doc$cov.df <- function() NULL


#----------------------------------------------
#' Get the distinct photographic conditions
#'
#' Returns a code{data.frame} with information about the different photographic
#' conditions under which the photographs, used to get the samples, were taken.
#' It is computed by the \code{\link{hvdvm$digest}} function.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$photo.conditions.df
#'  }
#'
#' @return A data.frame containing the following columns:
#'  \enumerate{
#'     \item\code{cond}
#'      Is the key identifier of the conditions, which is referenced by the
#'      variance and the covariance data in the \code{\link{hvdvm$var.df}} and
#'      \code{\link{hvdvm$cov.df}} variables.
#'
#'     \item\code{lighting}
#'     \item\code{iso}
#'     \item\code{shutter.speed}
#'     \item\code{aperture}
#'     \item\code{focal.length}
#'     \item\code{count}
#'     The number of samples having the given conditions.
#'  }
#'
#' This data prepared by the \code{\link{hvdvm$digest}} function selecting the
#' different combinations of values in the \code{.csv} input file. As the
#' \emph{hvdvm} technique requires pairs of images, only the conditions having
#' more than one image sample are included here.
#'
#' @seealso \code{\link{hvdvm$digest}}, \code{\link{hvdvm$cov.df}},
#'   \code{\link{hvdvm$var.df}}.
#'
#' @examples
#' \dontrun{
#'
#' # Find photographic conditions where more than 5 shots were taken
#' phcond <- my.hvdvm$photo.conditions.df
#' subset(phcond, count > 5)
#' }
#'
#' @seealso \code{\link{hvdvm$var.df}}, \code{\link{hvdvm$cov.df}}
#'
#' @name hvdvm$photo.conditions.df
#----------------------------------------------
hvdvm.doc$photo.conditions.df <- function() NULL


rm(hvdvm.doc)

#**********************
# End of Documentation
#**********************
