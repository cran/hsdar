\name{wavelength}
\alias{wavelength}
\alias{wavelength<-}
\alias{wavelength,Speclib-method}
\alias{wavelength<-,Speclib,data.frame-method}
\alias{wavelength<-,Speclib,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling wavelength
}
\description{
Returning and setting wavelength in Speclib
}
\usage{
\S4method{wavelength}{Speclib}(object)

\S4method{wavelength}{Speclib,data.frame}(object) <- value

\S4method{wavelength}{Speclib,numeric}(object) <- value
}
\arguments{
  \item{object}{
Object of class \code{Speclib}'.
}
  \item{value}{
Matrix containing wavelength values.
}
}
\value{
For \code{wavelength<-}, the updated object. Otherwise a numeric vector of the spectra in x is returned.
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

wavelength(spectral_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
