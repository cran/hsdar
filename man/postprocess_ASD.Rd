\name{postprocessASD}
\alias{postprocessASD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read ASD binary file
}
\description{
Read spectra stored in ASD binary files using the package 'asdreader'.
}
\usage{
postprocessASD(x, reference, removeCrossings = TRUE,
               correctReflectance = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class 'Speclib' containing spectra to be processed.
}
\item{reference}{
  Object of class 'Speclib' containing single reference spectrum (sensitivity of the white reference standard).
}
\item{removeCrossings}{
    Flag if channel crossings at 1000 nm and 1800 nm should be removed.
}
\item{correctReflectance}{
  Flag if reflectance values should be corrected using the spectrum of the reference.
}
}
\author{
Lukas Lehnert
}
\value{
  Object of class \code{Speclib}.
}
\seealso{
  \code{\link{speclib}}
}

\keyword{ utilities }
