% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/fit-model.r
\name{imgnoiser.model.predictions}
\alias{imgnoiser.model.predictions}
\title{Get model predictions}
\usage{
imgnoiser.model.predictions(
     model.src.data,
     fit,
     model.family,
     split.value,
     x.df
     )
}
\arguments{
\item{model.src.data}{The model data source as returned by
  \code{\link{imgnoiser.get.model.source.data}} with the model data source.}

\item{conf.level}{Confidence level used to compute \code{lpl} and \code{upl}
  in the resulting data frame.}

\item{model.family}{The \code{model.family} argument used in the call to
  \code{\link{imgnoiser.fit.model}} for the fitting of the \code{fit}
  argument.}

\item{split.value}{The factor value identifying the group of observation in
  the model data source for ehich a model was fitted. In the default model
  this corresponds to a channel label in the variance data.}

\item{x.df}{A data frame with the predictor data for which the predictions
  are required.}

\item{fit}{A model, returned by \code{\link{imgnoiser.fit.model}}, for which
  prediction is desired.}
}
\value{
A data.frame containing:

   \itemize{
     \item \code{pred.df[,1]} The predictor variable in the data model. For
     the default model (\code{standard} ), this corresponds to the
     \code{mean} column in the variance data.

     The range of values in this column spans the range of the original data
     used to fit the model. It contains 20 equally spaced values along the
     range of the source data, plus 10 values logarithmically equally spaced.
     If the range of the original data contains the zero or negative values,
     the predictor variable will contain 30 equally spaced values.

     \item \code{pred.df[,2]} The predicted variable according to the fitted
     model. For the \code{standard} default model, this corresponds to the
     \code{var} column in the variance data.

     \item \code{pred.df[,3]} The factor variable used to group the
     preditions. For the \code{standard} default model, this corresponds to
     the \code{channel} column in the variance data. This is the
     \code{split.value} argument.

     \item \code{fit.se} The prediction standard error.

     \item \code{lpl} The \emph{lower prediction confidence limit}.

     \item \code{upl} The \emph{upper prediction confidence limit}.
   }
}
\description{
Return predicted values from the models fitted with the
\code{\link{hvdvm$fit.model}} or \code{\link{vvm$fit.model}} function. It is
used internally and is exposed only to provide an easy way to extend the
package by adding new models.
}
\details{
This function is internally called by the package. It is not expected a
direct call to this function. It is made public to allow the user extend the
package providing other data model fitting families for the noise analysis.

This function is the default value of the \code{get.model.prediction} option.
The value of that option can be changed to refer to another function, with
the same signature as this, providing additional model predictors. This other
function might call this one for the data models already implemented here.

It is pretty unlikely need to set other value for the \code{get.model.prediction}
option on the basis of supporting new model families, because this function
uses the \code{stats::predict} function to get the model predictions and all
the well-known fitting models have sub classed that generic function to
support their predicitions (see the \code{predict} function).

Even though, the chance to override this function can be useful. For example,
for the case when the data model must be transformed to fulfill
pre-requisites of the fitting model, as in the case of removing
heteroscedasticity for a linear fitting. In such cases can be desired to
reverse the transformation in order to get the predictions in the original
domain of the data source.
}
\seealso{
\code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}, the
  option \code{get.model.prediction} of \code{\link{imgnoiser.option}},
  \code{\link{imgnoiser.get.model.source.data}},
  \code{\link{imgnoiser.fit.model}}
}

