\docType{class}
\name{colmap}
\alias{colmap}
\title{Color space conversion of raw image}
\description{
The \code{colmap} class contains tools for the color conversion from raw
images to other color spaces, including RGB ones.
}

\section{Methods}{

\describe{
\item{\code{new(
camera.metadata = stop("The 'camera.metadata' argument is missing.")
)}}{Create a new instance of the \code{colmap} class.}

\item{\code{find.raw.white.cct(
white.linear = stop("The 'white.linear' argument is missing."),
verbosity = 0
)}}{Find the CCT (in Kelvin degrees) corresponding to a camera raw white reference.}

\item{\code{get.conv.matrix.from.raw(
from.neutral.raw  = stop("The 'white.linear' argument is missing."),,
to.space='sRGB'
)}}{Get the color conversion matrix from camera raw to a given color space,
using for white-balance a given raw neutral color reference.}

\item{\code{prepare.to.rgb.conversions(
rgb.scale = 255,
RGGB.indices = c(1,2,3,4),
tone.curve = 'linear'
)}}{After a call to the \code{get.conv.matrix.from.raw()} function, prepares
the object (from a performance point of view) to make color conversions from
camera raw to the selected color space target. The color conversions will be
done with calls to the \code{convert.raw.to.rgb()} function.
}

\item{\code{convert.raw.to.rgb(
cfa = stop("The 'white.linear' argument is missing."),
is.neutral = FALSE
)}}{Convert raw channels to the selected color space.
}

}}

