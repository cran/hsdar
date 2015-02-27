\name{Nri-class}
\docType{class}
\alias{Nri-class}

\title{
* Nri class
}
\description{
Class to handle datasets containing normalized ratio indices of spectra.
}
\details{
Object with slots:
\itemize{
  \item{nri: }{Object of class \code{\linkS4class{DistMat3D}} containing nri values.}
  \item{fwhm: }{Vector or single numerical value giving the full-width-half-max value(s) for each band.}
  \item{wavelength:}{ Vector with wavelength information.}
  \item{dimnames: }{Character string containing band names used to calculate nri-values.}
  \item{multivariate: }{Character string defining the kind of test/model applied to the data. Only used after object has passed e.g. \code{\link[=glm.nri]{(g)lm.nri}}.}
}
}

\author{
Lukas Lehnert
}


\seealso{
\code{\linkS4class{Speclib}}
}
\keyword{classes}
