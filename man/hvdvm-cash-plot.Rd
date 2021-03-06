% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/noise-class.r
\name{hvdvm$plot}
\alias{hvdvm$plot}
\alias{vvm$plot}
\title{Plot a fitted model and its source data}
\arguments{
\item{model.name}{Name of the model whose prediction (see the \code{fit}
  argument) or confidence area (see the \code{confid} argument) are desired
  in the plot.}

\item{obs}{If TRUE the observations will be included in the plot.}

\item{print}{If TRUE the plot will be rendered in the output device. If you
  want to customize the plot before printing it, set this parameter to FALSE
  and use the returned ggplot2 value.}

\item{fit}{If TRUE the prediction intervals of the model referred with the
  \code{model.name} argument, will be plotted as a line for each channel.}

\item{x}{}

\item{y}{The default model uses the \code{mean} and \code{var} variables in
  the \code{var.df} data frame as the variables to plot in the x and y axes.
  However, you can change the plot to use in those axes the expressions you
  want. For example, you may want to plot the SNR (signal to noise ratio), in
  such case use \code{y = mean/sqrt(var)}.}

\item{pred.int}{If TRUE the area between the predictions confidence limits, of
  the model referred with the \code{model.name} argument, will be drawn with
  a semi-transparent color.}

\item{tlab}{The plot main title. If it is not given, the title in the model
  data source will be used. A NULL value means no title is desired.}

\item{slab}{The plot subt-title. If it is not given the plot won't have a
  sub-title.}

\item{xlab}{The plot x axis title. If it is not given, the corresponding
  label in the model data source will be used. A NULL value means no axis
  title is desired.}

\item{ylab}{The plot y axis title. If it is not given, the corresponding
  label in the model data source will be used. A NULL value means no axis
  title is desired.}

\item{xlim}{A vector with two values, where the second is greater than the
  first. The limits for the x axis. See \code{warnings} to avoid the warnings
  when using this parameter.}

\item{ylim}{A vector with two values, where the second is greater than the
  first. The limits for the y axis. See \code{warnings} to avoid the warnings
  when using this parameter.}

\item{with}{A formula to filter the data to show in the plot. You can use a
an expression or a function. The formula and the function must return a vector
of logical values selecting the the desired rows from \code{var.df}.
For example the argument \code{with = ~ channel != 'Green Avg'} will
exclude the channels with the \code{'Green Avg'} label.}

\item{warnings}{If TRUE, suppress the warnings when printing the plot.

When using the \code{xlim} or \code{ylim} argument, ggplot2 throws some
warnings about the data not being included in the plot because of those
limits. That can be annoying, and by default all the warnings are turned off.
You may want to change this default value if for some reason you want to get
any warning that is being suppressed by this default.}
}
\value{
Invisibly returns a ggplot2 object which can be used to customize the
  plot.
}
\description{
Plot a fitted model and its source data
}
\section{Usage}{

 \preformatted{
  hvdvm$plot(
     model.name = FALSE,
     obs = TRUE,
     print = TRUE,
     fit = FALSE,
     pred.int = FALSE,
     x = NULL,
     y = NULL,
     tlab = NULL,
     slab = NULL,
     xlab = NULL,
     ylab = NULL,
     xlim = NULL,
     ylim = NULL,
     with = NULL,
     warnings = FALSE
     )

  vvm$plot(
     model.name = FALSE,
     obs = TRUE,
     print = TRUE,
     fit = FALSE,
     pred.int = FALSE,
     x = NULL,
     y = NULL,
     tlab = NULL,
     slab = NULL,
     xlab = NULL,
     ylab = NULL,
     xlim = NULL,
     ylim = NULL,
     with = NULL,
     warnings = FALSE
     )
 }
}
\examples{
\dontrun{

# Plot the observations in the standrad data model
my.hvdvm$plot()

# Plot the model named 'standard' using a customized title
my.hvdvm$plot(
   model.name = TRUE,
   main='Nikon D7000 - ISO 100'
   subt = 'Half Var Delta versus Mean'
   )
}
}

