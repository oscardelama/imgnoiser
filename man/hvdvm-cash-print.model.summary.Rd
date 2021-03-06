% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/noise-class.r
\name{hvdvm$print.model.summary}
\alias{hvdvm$print.model.summary}
\alias{vvm$print.model.summary}
\title{Print a fitted model summary}
\arguments{
\item{model.name}{The name of the fitted model whose summary is desired.}

\item{select}{A vector with the indices or labels of the channels whose
  summary is desired.}

\item{...}{Additional parameters for the \code{summary} which is
  called by this function to get the model fitting summary information.}
}
\value{
Prints a report with four summaries, one for each fitted channel. The
  report starts with a header with the model name and the call sentence used
  to fit the models. Each summary is subtitled naming the corresponding
  channel label. The exact summary content depends on the model family used
  to fit the model.
}
\description{
Print the summary of a fitted model.
}
\details{
In the default (\code{std.var}) model, the variance (\code{var}) is predicted by
the \emph{mean} for each \emph{channel} in the image samples. Therefore,
there is a model fitted for each channel, and consequently the summary
printed by this function contains four subsections, with the summary of the
model fitted for each channel.
}
\section{Usage}{

 \preformatted{
  vvm$print.model.summary(
     model.name = imgnoiser.option('fit.model.name'),
     select = NULL,
     ...
     )

  hvdvm$print.model.summary(
     model.name = imgnoiser.option('fit.model.name'),
     select = NULL,
     ...
     )
 }
}
\examples{
\dontrun{

# Print the model summary for the 'standard' fitting
my.hvdvm$print.model.summary()
# Print the model for the 'weigthed' model including the correlation matrix
my.hvdvm$print.model.summary('weigthed', correlation = TRUE)
}
}
\seealso{
\code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}
}

