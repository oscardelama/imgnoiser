% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/fit-model.r
\name{imgnoiser.fit.model}
\alias{imgnoiser.fit.model}
\title{Fit data to a model}
\usage{
imgnoiser.fit.model(
     model.family,
     model.formula,
     dom.data,
     model.call.params
     )
}
\arguments{
\item{model.family}{The name of the model family that will be fit over the
  given data. The supported model families are:

   \itemize{
     \item \code{lm} Linear models supported by the \code{stats::lm}
     function.

     \item \code{lmrob} MM-type estimators for linear models, supported by
     the \code{stats::lmrob} function.

     \item \code{smooth.spline} Cubic smoothing spline fitting, supported by
     the \code{stats::smooth.spline} function.
   }

  By default, the functions supporting these model families are called only
  with the model formula (or just \code{x} and \code{y} for the case of
  smooth.spline) and the model data. All the additional arguments, supported
  by the function behind each model family, can be specified by the user as
  elements of the \code{...} argument in the calling to the function
  \code{fit.model} in the classes \code{hvdvm} and \code{vvm}.}

\item{model.formula}{An object of the class \code{"formula"}, to be used in
  the fitting process, like -for example- the \code{formula} argument when
  fitting a linear model using the function \code{lm}.

  This formula is either, given directly by the user in the argument
  \code{formula}, in the call to the \code{fit.model} function, or indirectly
  when the user does not provide a formula but the \code{degree} parameter,
  in which case the formula is a polynomial, with the given degree, on the
  predictor variable \code{x}.

  Some model fitting models, as \code{smooth.spline} don't support formulas,
  in such cases this argument will be NULL. The models that use the
  \code{formula} and the \code{data} arguments are in the character vector
  given by the \code{is.linear.model} option of the
  \code{\link{imgnoiser.option}} function.}

\item{dom.data}{The data that will be fit to the model. It is a data frame
  where the first column is the predictor (\code{x}) variable and the second
  is the predicted one (\code{y}).}

\item{model.call.params}{A list with other arguments specific to the selected
  model family. They pass as the triple dot argument in the call to the
  \code{fit.model} function. For technical reasons that triple dot argument
  is transformed into this list.}
}
\value{
The object representing the model fitting, as is returned by the
  functions supporting the model families.
}
\description{
Returns an object representing a model fiting as -for example- the function
\code{lm} does. This function is exposed only to provide an easy way to
extend the package adding support to additional fitting models.
}
\details{
This function is internally called by the \code{fit.model} function in the
classes \code{hvdvm} and \code{vvm}, it is not expected a direct call to this
function. It is made public only to allow the user to extend it, allowing the
fitting of data to additional model families.

This function is the default value of the \code{fit.model} option. The value
of that option can be changed to refer to another function, with the same
signature as this, providing additional model families. This other function
might call this one for the models already supported here.
}
\seealso{
\code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}, the
  option \code{fit.model} of \code{\link{imgnoiser.option}},
  \code{\link{imgnoiser.get.model.source.data}},
  \code{\link{imgnoiser.model.predictions}}
}

