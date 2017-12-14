\name{Specfeat-class}
\docType{class}
\alias{Specfeat-class}

\title{
* Specfeat class
}
\description{
Class to handle spectral feature data. Spectral features are absorption (transmission or reflection) bands defined e.g. by continuum removal (see \code{\link{transformSpeclib}}).
}
\details{
Class extends \code{Speclib}-class and adds two additional slots:
\itemize{
  \item{features: }{List containing the spectra according to the features.}
  \item{featureLimits: }{List containing limits of features defined by \code{\link{specfeat}}.}
}
}
\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in \pkg{hsdar}.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\linkS4class{Speclib}}, \code{\link{specfeat}}
}
\keyword{classes}
