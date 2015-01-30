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


#-----------------------------
#' Returns a model data
#'
#' Returns the data corresponding to a given model name. It is used internally
#' and is exposed only to provide to the user a easy way to extend it adding
#' new data models.
#'
#' This function is internally called by the \code{fit.model} function in the
#' classes \code{hvdvm} and \code{vvm}. It is not inteded to be called directly.
#' It is made public to allow the user extend it providing other data models for
#' the noise analysis.
#'
#' This function is the default value of the \code{get.model.src.data} option.
#' The value of that option can be changed to refer to another function, with
#' the same signature as this, providing additional data models. This other
#' function might call this one for the data models already implemented here.
#'
#' @usage
#'   imgnoiser.get.model.source.data(
#'      model.name,
#'      noise.obj
#'      )
#'
#' @param model.name The name of the required data. Actually, the package only
#'   supports the data model named 'standard' which is the default value for
#'   this argument, see details bout this in the description of the return
#'   value.
#'
#' @param noise.obj An instance, either of the class \code{\link{hvdvm}}
#'   or \code{\link{vvm}}. This object provide the data from which the
#'   data model will be constructed.
#'
#' @return A list with the following elements:
#'  \itemize{
#'
#'    \item \code{data} A data frame with the following variables:
#'
#'      \itemize{
#'          \item \code{x} Numeric values. They will be plotted along the x axis
#'          and will be used as predictor variable for model fitting.
#'
#'          \item \code{y} Numeric values. They will be plotted along the y axis
#'          and will be used as the predicted variable for model fitting.
#'
#'          \item \code{split.by} A factor. This variable is optional. If present
#'          it will be used to split the (x, y) values in as many groups as
#'          different values are in this variable.
#'      }
#'
#'    \item \code{label} A list with the labels for the elements in the data
#'    model and for the its representation in a plot.
#'
#'      \itemize{
#'          \item \code{term} A named vector with the names corresponding to the
#'          variables in the data frame.
#'
#'          The names of the elements in the vector must be
#'          \code{c('x','y','split.by')}, and as values the corresponding labels
#'          for those variables, for example \code{c('Mean','Var','Channel')},
#'          so the value of \code{term['x']} is \code{'Mean'}, the label for the
#'          'x' variable.
#'
#'          \item \code{xlab} The title for the x axis in the plot.
#'          \item \code{ylab} The title for the y axis in the plot.
#'          \item \code{main} The main plot title.
#'      }
#'  }
#'
#'  For the \emph{standard} model, which actually is the only one supported, the
#'  data comes from the variance information in the instance object given in the
#'  \code{noise.obj} argument, get through the use of the function
#'  \code{noise.obj$var.df}.
#'
#'  The \code{x} and \code{y} values in the function return value are the
#'  \code{mean} and the \code{var} variables in the variance data, and as
#'  \code{split.by} is used the \code{channel} variable.
#'
#'  The labels (\code{xlab}, \code{ylab}) and title (\code{main}) are customized
#'  according the class of the \code{noise.obj} argument.
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}, the
#'   option \code{get.model.src.data} of \code{\link{imgnoiser.option}},
#'   \code{\link{imgnoiser.fit.model}},
#'   \code{\link{imgnoiser.model.predictions}}
#'
#' @export
#-----------------------------
imgnoiser.get.model.source.data <- function(model.name, noise.obj) {

  if (model.name != 'std.var')
    stop('The source model name is unknown.')

  if ('vvm' %in% class(noise.obj)) {
    class.labels <- list(
       'ylab' = 'Variance (ADU^2)'
      ,'main' = 'Variance vs Mean'
      )
  }
  else if ('hvdvm' %in% class(noise.obj)) {
    class.labels <- list(
         'ylab' = 'Half Var Delta (ADU^2)'
        ,'main' = 'Half Var Delta vs Mean'
        )
  }

  var.df <- noise.obj$var.df
  return(list(
          'data' = data.frame(
             'x' = var.df$mean
            ,'y' = var.df$var
            ,'split.by' = var.df$channel
            ,row.names = NULL
          )
        ,'name' = model.name
        ,'label' = c(
              list(
                 'term' = setNames(c('mean','var','channel'), c('x','y','split.by'))
                ,'xlab' = 'Mean (ADU)'
                )
              ,class.labels
          )
    ))
}

#-----------------------------
#' Fit data to a model
#'
#' Returns an object representing a model fiting as -for example- the function
#' \code{lm} does. This function is exposed only to provide an easy way to
#' extend the package adding support to additional fitting models.
#'
#' This function is internally called by the \code{fit.model} function in the
#' classes \code{hvdvm} and \code{vvm}, it is not expected a direct call to this
#' function. It is made public only to allow the user to extend it, allowing the
#' fitting of data to additional model families.
#'
#' This function is the default value of the \code{fit.model} option. The value
#' of that option can be changed to refer to another function, with the same
#' signature as this, providing additional model families. This other function
#' might call this one for the models already supported here.
#'
#' @usage
#'   imgnoiser.fit.model(
#'      model.family,
#'      model.formula,
#'      dom.data,
#'      model.call.params
#'      )
#'
#' @param model.family The name of the model family that will be fit over the
#'   given data. The supported model families are:
#'
#'    \itemize{
#'      \item \code{lm} Linear models supported by the \code{stats::lm}
#'      function.
#'
#'      \item \code{lmrob} MM-type estimators for linear models, supported by
#'      the \code{stats::lmrob} function.
#'
#'      \item \code{smooth.spline} Cubic smoothing spline fitting, supported by
#'      the \code{stats::smooth.spline} function.
#'    }
#'
#'   By default, the functions supporting these model families are called only
#'   with the model formula (or just \code{x} and \code{y} for the case of
#'   smooth.spline) and the model data. All the additional arguments, supported
#'   by the function behind each model family, can be specified by the user as
#'   elements of the \code{...} argument in the calling to the function
#'   \code{fit.model} in the classes \code{hvdvm} and \code{vvm}.
#'
#' @param model.formula An object of the class \code{"formula"}, to be used in
#'   the fitting process, like -for example- the \code{formula} argument when
#'   fitting a linear model using the function \code{lm}.
#'
#'   This formula is either, given directly by the user in the argument
#'   \code{formula}, in the call to the \code{fit.model} function, or indirectly
#'   when the user does not provide a formula but the \code{degree} parameter,
#'   in which case the formula is a polynomial, with the given degree, on the
#'   predictor variable \code{x}.
#'
#'   Some model fitting models, as \code{smooth.spline} don't support formulas,
#'   in such cases this argument will be NULL. The models that use the
#'   \code{formula} and the \code{data} arguments are in the character vector
#'   given by the \code{is.linear.model} option of the
#'   \code{\link{imgnoiser.option}} function.
#'
#' @param dom.data The data that will be fit to the model. It is a data frame
#'   where the first column is the predictor (\code{x}) variable and the second
#'   is the predicted one (\code{y}).
#'
#' @param model.call.params A list with other arguments specific to the selected
#'   model family. They pass as the triple dot argument in the call to the
#'   \code{fit.model} function. For technical reasons that triple dot argument
#'   is transformed into this list.
#'
#' @return The object representing the model fitting, as is returned by the
#'   functions supporting the model families.
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}, the
#'   option \code{fit.model} of \code{\link{imgnoiser.option}},
#'   \code{\link{imgnoiser.get.model.source.data}},
#'   \code{\link{imgnoiser.model.predictions}}
#'
#' @export
#-----------------------------
imgnoiser.fit.model <- function(model.family, model.formula, dom.data, model.call.params) {

  switch(
    EXPR = model.family

    # Fit a linear models without
    ,lm = do.call(lm, c(list(formula=model.formula, data=dom.data), model.call.params))

    # Fit a robust regression
    ,lmrob = {
      #require(robustbase)
      if (requireNamespace("robustbase", quietly = TRUE))
        do.call(robustbase::lmrob, c(list(formula=model.formula, data=dom.data), model.call.params))
      else
        stop("Package robustbase needed for this function to work. Please install it."
             ,call. = FALSE)
    }

    # Fit a smooth spline
    ,smooth.spline = {
      do.call(stats::smooth.spline, c(list(x=dom.data[,1L], y=dom.data[,2L]), model.call.params))
    }

    ,stop("The model family argument is unexpected/unknown.")
  )

}

#-----------------------------
# Fit a model to the data.
#
# fit.model(model.src.data, split.value, lazy.formula = NULL, model.family, degree, ...)
#
#' @importFrom data.table setnames
#-----------------------------
util.fit.model <- function(model.src.data, split.value, lazy.formula = NULL, model.family, degree, ...) {

  # Get the model data
  dom.data <- model.src.data[['data']]
  if (!is.null(split.value))
    dom.data <- subset(dom.data, dom.data$split.by == split.value)
  # Use the model names
  # names(dom.data) <- model.src.data[['label']][['term']]
  data.table::setnames(dom.data,  model.src.data[['label']][['term']])

  dots.lazy <- lazyeval::lazy_dots(...)

  #-- Prepare the model call in text form
  model.call.params.txt <- get.param.list(dots.lazy)
  if ('lazy' %in% class(lazy.formula)) {
    formula.txt <- deparse(lazy.formula[['expr']])
    model.call.txt <- paste0(model.family,'(formula = ',formula.txt,', data = model.src(<%MODEL%>)', model.call.params.txt,')')
    # Build the formula with bindings to the model data
    model.formula <- lazyeval::lazy_eval(lazy.formula, dom.data)
  } else {
    model.formula <- lazy.formula
    formula.member <- as.character(lazy.formula)
    if (length(formula.member) > 0) {
      formula.txt <- paste(formula.member[2], formula.member[1], formula.member[3])
      model.call.txt <- paste0(model.family,'(formula = ', formula.txt,', data = model.src(<%MODEL%>)',  model.call.params.txt,')')
    } else
      model.call.txt <- paste0(model.family,'(data = model.src(<%MODEL%>)',  model.call.params.txt,')')
  }
  #--

  # Evaluate the 3dots argument with the model data as environment
  # browser()
  model.call.params <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), data = dom.data)
  # Get the model fitter function
  model.fitter.func <- imgnoiser.option('fit.model')
  # fit.model(model.family, model.formula, dom.data, model.call.params)
  fit <- model.fitter.func(model.family, model.formula, dom.data, model.call.params)

  return(list('model'= fit, 'call'= model.call.txt, 'lazy.dots' = dots.lazy))
}

#-----------------------------
#' Get model predictions
#'
#' Return predicted values from the models fitted with the
#' \code{\link{hvdvm$fit.model}} or \code{\link{vvm$fit.model}} function. It is
#' used internally and is exposed only to provide an easy way to extend the
#' package by adding new models.
#'
#' This function is internally called by the package. It is not expected a
#' direct call to this function. It is made public to allow the user extend the
#' package providing other data model fitting families for the noise analysis.
#'
#' This function is the default value of the \code{get.model.prediction} option.
#' The value of that option can be changed to refer to another function, with
#' the same signature as this, providing additional model predictors. This other
#' function might call this one for the data models already implemented here.
#'
#' It is pretty unlikely need to set other value for the \code{get.model.prediction}
#' option on the basis of supporting new model families, because this function
#' uses the \code{stats::predict} function to get the model predictions and all
#' the well-known fitting models have sub classed that generic function to
#' support their predicitions (see the \code{predict} function).
#'
#' Even though, the chance to override this function can be useful. For example,
#' for the case when the data model must be transformed to fulfill
#' pre-requisites of the fitting model, as in the case of removing
#' heteroscedasticity for a linear fitting. In such cases can be desired to
#' reverse the transformation in order to get the predictions in the original
#' domain of the data source.
#'
#' @usage
#'   imgnoiser.model.predictions(
#'      model.src.data,
#'      fit,
#'      model.family,
#'      split.value,
#'      x.df
#'      )
#'
#' @param model.src.data The model data source as returned by
#'   \code{\link{imgnoiser.get.model.source.data}} with the model data source.
#'
#' @param fit A model, returned by \code{\link{imgnoiser.fit.model}}, for which
#'   prediction is desired.
#'
#' @param model.family The \code{model.family} argument used in the call to
#'   \code{\link{imgnoiser.fit.model}} for the fitting of the \code{fit}
#'   argument.
#'
#' @param conf.level Confidence level used to compute \code{lpl} and \code{upl}
#'   in the resulting data frame.
#'
#' @param split.value The factor value identifying the group of observation in
#'   the model data source for ehich a model was fitted. In the default model
#'   this corresponds to a channel label in the variance data.
#'
#' @param x.df A data frame with the predictor data for which the predictions
#'   are required.
#'
#' @return A data.frame containing:
#'
#'    \itemize{
#'      \item \code{pred.df[,1]} The predictor variable in the data model. For
#'      the default model (\code{standard} ), this corresponds to the
#'      \code{mean} column in the variance data.
#'
#'      The range of values in this column spans the range of the original data
#'      used to fit the model. It contains 20 equally spaced values along the
#'      range of the source data, plus 10 values logarithmically equally spaced.
#'      If the range of the original data contains the zero or negative values,
#'      the predictor variable will contain 30 equally spaced values.
#'
#'      \item \code{pred.df[,2]} The predicted variable according to the fitted
#'      model. For the \code{standard} default model, this corresponds to the
#'      \code{var} column in the variance data.
#'
#'      \item \code{pred.df[,3]} The factor variable used to group the
#'      preditions. For the \code{standard} default model, this corresponds to
#'      the \code{channel} column in the variance data. This is the
#'      \code{split.value} argument.
#'
#'      \item \code{fit.se} The prediction standard error.
#'
#'      \item \code{lpl} The \emph{lower prediction confidence limit}.
#'
#'      \item \code{upl} The \emph{upper prediction confidence limit}.
#'    }
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}, the
#'   option \code{get.model.prediction} of \code{\link{imgnoiser.option}},
#'   \code{\link{imgnoiser.get.model.source.data}},
#'   \code{\link{imgnoiser.fit.model}}
#'
#' @importFrom data.table setnames
#' @export
#-----------------------------
imgnoiser.model.predictions <- function(
        model.src.data
        ,model.obj
        ,conf.level
        ,model.family
        ,split.value
        ,x.df
        ,lazy.dots
) {

  # Use domain names
  data.table::setnames(x.df, model.src.data[['label']][['term']]['x'])

  # Use the model weights also in the predictions
  if ('weights' %in% names(lazy.dots)) {
    pred.weights <- lazyeval::lazy_eval(lazy.dots[['weights']], x.df)
    pred <- stats::predict(model.obj, newdata = x.df, interval='prediction',
                           weights=pred.weights, se.fit=TRUE, level = conf.level)
  }
  else
    pred <- stats::predict(model.obj, newdata = x.df, interval='prediction',
                           se.fit=TRUE, level = conf.level)

  if (model.family == 'smooth.spline') {
    result <- data.frame('x' = pred[['x']]
                         ,'y' = pred[['y']]
                         ,'split.by'  = split.value
                         )
    data.table::setnames(result, 1L:2L, model.src.data[['label']][['term']][1L:2L])
  } else {
    pred.fit.df <- as.data.frame(pred$fit)
    result <-
        data.frame(
           'x'         = x.df[,1L]
          ,'y'         = pred.fit.df$fit
          ,'split.by'  = split.value
          ,'fit.se'    = pred$se.fit
          ,'lpl'       = pred.fit.df$lwr
          ,'upl'       = pred.fit.df$upr
        )
    data.table::setnames(result, 1L:3L, model.src.data[['label']][['term']][1L:3L])
  }
  result;
}

#### Helper functions (non public) ######

#-----------------------------
# get.param.list(lazy.dots)
#
# Get a character string with the parameters and arguments in a triple dots parameter.
#
# Returns a list in the form of 'parameter = argument' separated by commas. As
# the arguments have not been evaluated they have the original form as when the
# call was done, like in 'weigths = 1/mean^2'.
#
# @param lazy.dots Triple dots (ellipsis) en lazy form in the sense of the
#   lazyeval package.
#
# @return A character string with the parameters and arguments in the lazy.dots
#   argument.
#-----------------------------
get.param.list <- function(lazy.dots) {
  # Get the dots values as text
  list.values.txt <- lapply(lazy.dots, function(x) deparse(x[['expr']]))
  param.names <- names(list.values.txt)
  result <- ''
  for (param.ix in seq_along(list.values.txt))
    result <- paste0(result, ', ', param.names[param.ix], ' = ', list.values.txt[[param.ix]])

  result
}

#-----------------------------
# get.model.formula(model.src.data, lazy.formula, degree, model.family)
#
# Builds and return the formula that will be used in the model fitting for the
# cases when the specified model.family allows it.
#
# It is illegal that both arguments,lazy.formula and degree are considered as
# not given. At least one of them should have a valid value. Otherwise, this
# function will emit a stop reporting that error.
#
# @param model.src.data a list with the structure used in the noise.var class to
#   save source data for the model fitting. This data is saved in a private
#   member of the noise.var class in
#   noise.var$private$model[[model.name]][['source']].
#
# @param lazy.formula The formula (in lazy form) used in the call to the 'fit
#   the model' function. If it is not neither NULL nor NA is exactly what this
#   function will return. Otherwise, it will prepeare a formula based in the
#   degree argument and the data source variables whose names are in
#   model.src.data[['label']][['term']].
#
# @param degree The degree of the polynomial in 'x' that will be constructed as
#   predictor of the 'y' term in the model.src.data argument.
#
# @param model.family The family of the model that will be used to fit the data.
#   For example 'lm' for the function stat::lm(). If the model family does not
#   allow this kind of formula, as is the case for the smooth.spline() family,
#   the argument lazy.formula is expected to be NULL or NA and the degree will
#   be ignored. In this case this function will just return NULL.
#-----------------------------
get.model.formula <- function(model.src.data, lazy.formula=NULL, degree, model.family) {

  linear.models <- imgnoiser.option('is.linear.model')
  result <- NULL

  if (model.family %nin% linear.models) {
    if (is.given(lazy.formula))
      stop('The given model family does not accept formulas.')
  } else {

    if (!is.null(lazy.formula))
      result <- lazy.formula
    else {

      # Default degree for linear models is 1
      if (!is.given(degree))
        return (NULL)
      else {
        degree <- vector.alike(degree, 1L, type='i')
        check.all.whole.numbers(degree)
        if (degree < 1L)
          stop('Invalid degree argument.')
      }

      #-- Prepare the formula
      dom.data <- model.src.data[['data']]
      dom.terms <- model.src.data[['label']][['term']]

      if (degree < 3L) {
        formula.txt <- paste(dom.terms['y'], '~', dom.terms['x'])
        if (degree == 2L)
          formula.txt <- paste0(formula.txt, ' + I(', dom.terms['x'], '^2)')
      } else
        formula.txt <- paste0(dom.terms['y'], ' ~ poly(', dom.terms['x'],', ', degree, ', raw=TRUE)')

      formula.txt <- paste0('lazyeval::lazy(',formula.txt, ')')
      expr <- parse(text = formula.txt)
      result <- eval(expr)
    }
  }
  result
}

#-----------------------------
# function build.model.grid(x)
#
# Returns a 'model grid', which is a data.frame with values for the predictor
# variable that will be used for the plotting of a fitted model. Through the use
# of the fitted model, the response variable will be predicted for the predictor
# values returned by this function. Actually, this function supports only one
# predictor variable.
#
# The function returns 20 'x' values, equally spaced along the range of the x
# argument, plus 10 values equally logarithmically spaced. If the range of x
# contains the zero or negative values, the function just returns 30 equally
# spaced values.
#
# @param model.src.data The source data that was used to fit the model. This
#   function analyzes the
#-----------------------------
build.model.grid <- function(x) {

  # Build a grid with 30 observation points to evaluate predictions
  # from the fitted models
  obs.points <- function(.data) {
    range <- range(.data, na.rm = TRUE)
    min <- range[1L]
    max <- range[2L]
    if (min <= 0) {
      equally.spaced.linear.len <- 35
      log.points <- numeric()
    } else {
      log.points <- seq(log(min), log(max), length = 12L)
      log.points <- log.points[c(-1L,-12L)]
      equally.spaced.linear.len <- 25L
    }
    points <- c(exp(log.points), seq(min, max, length = equally.spaced.linear.len))
    return (sort(points))
  }

  dplyr::tbl_df(data.frame('x' = obs.points(x), row.names = NULL))
}

