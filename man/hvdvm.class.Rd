\name{hvdvm}
\alias{hvdvm.class}
\title{Analysis of raw image noise using the 'hvdvm' technique.}
\description{
The \code{hvdvm} class contains tools for the analysis, study and
plotting of the half variance of delta images for the study of noise in raw
image files.

You can read the "Introduction to the \emph{hvdvm} Technique" vignette for an
introduction to the subject and the "\emph{hvdvm} Class Usage" vignette for a
more detailed description of the general workflow using this class.
}
\section{Methods}{

 \describe{
    \item{\code{
      \link{hvdvm$new}(
         channel.labels = imgnoiser.option('channel.labels'),
         green.channels = imgnoiser.option('green.channels'),
         has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern'),
         avg.green.label = imgnoiser.option('avg.green.channel.label')
      )}}{
      Create a new instance of the hvdvm class.
      }

   \item{\code{
      \link{hvdvm$digest}(
        photo.conds.file = stop("The 'photo.conds.file' argument is required."),
        crop.files.path = './'
      )}}{
      Process raw image files with the hvdvm technique.
      }

   \item{\code{
      \link{hvdvm$get.var}
      }}{
      Get the computed variance data.
      }

   \item{\code{
      \link{hvdvm$get.cov}
      }}{
      Get the computed covariance data.
      }

   \item{\code{
      \link{photo.conditions.df}
      }}{
      Get the different photographic conditions in the raw image files.
      }

   \item{\code{
      \link{hvdvm$channel.labels}
      }}{
      Get the channel labels.
      }

   \item{\code{
      \link{hvdvm$fit.model}(
        model.name = imgnoiser.option('fit.model.name'),
        model.family = imgnoiser.option('fit.model.family'),
        degree = 1L,
        formula = NULL,
        model.data.name = imgnoiser.option('fit.model.data'),
        ...
      )}}{
      Fit a model to the noise data.
      }

   \item{\code{
      \link{hvdvm$print.model.summary}(
        model.name = imgnoiser.option('fit.model.name'),
        select = NULL,
        ...
        )}}{
        Print the summary of a fitted model.
        }

   \item{\code{
      \link{hvdvm$get.model}(
        model.name = imgnoiser.option('fit.model.name'),
        select = NULL
      )}}{
      Get a list with the objects fitted in a model.
      }

   \item{\code{
      \link{hvdvm$remove.model}( /* pending */
        model.name = stop("A 'model.name' argument is required.")
      )}}{
      Remove a model fitted with a given name.
      }

   \item{\code{
      \link{hvdvm$get.model.predictions}(
        model.name = imgnoiser.option('fit.model.name')
      )}}{
      Get the values predicted from a given fitted model.
      }

   \item{\code{
      \link{hvdvm$model.list}
      }}{
      Get a list with the currently fitted models.
      }

   \item{\code{
      \link{hvdvm$exists.model}(
        model.name = imgnoiser.option('fit.model.name')
      )}}{
      Find if a model with a given name has already been fitted.
      }

   \item{\code{
      \link{hvdvm$plot}(
        obs = TRUE,
        print = TRUE,
        model.name = FALSE,
        fit = TRUE,
        confid = FALSE,
        main = NA,
        subt = NA,
        xlab = NA,
        ylab = NA,
      )}}{
      Plot the computed data and a fitted model.
      }

 }
}

