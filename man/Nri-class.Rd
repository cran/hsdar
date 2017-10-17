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
  \item{wavelength: }{Vector with wavelength information.}
  \item{dimnames: }{Character vector containing band names used to calculate nri-values.}
  \item{multivariate: }{List defining the kind of test/model applied to the data and the model data. Only used after object has passed e.g. \code{\link[=glm.nri]{(g)lm.nri}}.}
  \item{SI: }{Data.frame containing additional data}
  \item{usagehistory: }{Vector giving information on history of usage of the object.}
}
}
\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in hsdar.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\linkS4class{Speclib}}
}
\keyword{classes}
