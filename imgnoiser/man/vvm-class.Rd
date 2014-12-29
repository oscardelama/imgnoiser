\name{vvm-class}
\docType{class}
\alias{vvm}
\title{Measurement and study of raw image noise.}
\description{
The \code{vvm} class contains tools for the measurement, analysis and study of
the mean, variance and covariance of pixel values in raw image files.

You can read the \emph{"\code{vvm} Class Introduction"} vignette for an
introduction to the subject and the \emph{"\code{hvdvm} and \code{vvm} vvm
Class Usage"} vignette for additional details about the general
workflow using this class.
}

\section{Methods}{

  \describe{

    \item{ \preformatted{vvm$new(
channel.labels = imgnoiser.option('channel.labels'),
green.channels = imgnoiser.option('green.channels'),
has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern'),
avg.green.label = imgnoiser.option('avg.green.channel.label')
)}}{Create a new instance of the vvm class.}

    \item{\preformatted{vvm$digest(
img.file.name = stop("The 'img.file.name' argument is required"),
img.file.count = NULL,
file.path = './',
img.file.name.ext = '.fit'
)}}{Collect noise measurements from raw image sample files.}

    \item{\code{vvm$var.df}
    }{A data frame with the computed variance data.}

    \item{\code{vvm$cov.df}
    }{A data frame with the computed covariance data.}

    \item{\code{vvm$channel.labels}
    }{Get the channel labels.}


 }
}
