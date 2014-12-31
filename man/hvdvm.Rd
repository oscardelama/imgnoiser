\docType{class}
\name{hvdvm}
\alias{hvdvm}
\alias{hvdvm-class}
\title{Analysis of noise in raw images using the "hvdvm" technique}
\description{
The \code{hvdvm} class contains tools for the measurement, analysis and
study of noise through the use of the half-var-delta versus mean (\acronym{hvdvm})
technique.

You can read an introduction to this class usage in the vignette
\emph{"hvdvm Class Introduction"}:

\code{vignette('hvdvm-introduction', package = 'imgnoiser')}
}

\section{Methods}{

\describe{
\item{\code{new(
channel.labels = imgnoiser.option("channel.labels"),
green.channels = imgnoiser.option("green.channels"),
has.RGGB.pattern = imgnoiser.option("has.RGGB.pattern"),
avg.green.label = imgnoiser.option("avg.green.channel.label")
)}}{Create a new instance of the \code{hvdvm} class.}

\item{\code{digest(
photo.conds.file = stop("The 'photo.conds.file' argument is required."),
crop.files.path = './'
)}}{Compute and collect statistics from the raw images files.}

\item{\code{var.df}}{Get the resulting noise variance data.}

\item{\code{cov.df}}{Get the resulting noise covariance data.}

\item{\code{photo.conditions.df}}{Get the distinct photographic conditions.}

\item{\code{channel.labels}}{Get the channel labels.}

\item{\code{green.channels}}{Get the green channels indices.}

\item{\code{fit.model(
model.name = imgnoiser.option("fit.model.name"),
model.family = imgnoiser.option("fit.model.family"),
degree = 2L,
formula = NULL,
model.data.name = imgnoiser.option("fit.model.data"),
...
)}}{Fit a model to the noise data.}

\item{\code{print.model.summary(
model.name = imgnoiser.option("fit.model.name"),
select = NULL,
...
)}}{Print a fitted model summary.}

\item{\code{get.model.predictions(
model.name = imgnoiser.option("fit.model.name")
)}}{Get the values predicted by a fitted model.}

\item{\code{get.model(
model.name = imgnoiser.option("fit.model.name"),
select = NULL
)}}{Get a fitted model object.}

\item{\code{exists.model(
model.name = imgnoiser.option("fit.model.name")
)}}{Find if a model with a given name has already been fitted.}

\item{\code{model.list}}{Get a list with the fitted models.}

\item{\code{remove.model(
model.name = stop("A model name argument is required.")
)}}{Remove a fitted model.}

\item{\code{plot(
model.name = FALSE,
obs = TRUE,
print = TRUE,
fit = TRUE,
confid = FALSE,
main = NA,
subt = NA,
xlab = NA,
ylab = NA,
xlim = NULL,
ylim = NULL,
warnings = FALSE
)}}{Plot a fitted model and its source data.}
}}

