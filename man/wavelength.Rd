\name{wavelength}
\alias{wavelength}
\alias{wavelength<-}
\alias{wavelength,Speclib-method}
\alias{wavelength<-,Speclib,data.frame-method}
\alias{wavelength<-,Speclib,numeric-method}
\alias{fwhm<-}
\alias{fwhm<-,Speclib,numeric-method}
\alias{fwhm}
\alias{fwhm,Speclib-method}

\alias{wavelength,HyperSpecRaster-method}
\alias{wavelength<-,HyperSpecRaster,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling wavelength and fwhm 
}
\description{
Methods to access and set wavelength (band center) and full-width-half-max (fwhm) values for class \code{Speclib}.
}
\usage{
\S4method{wavelength}{Speclib}(object)

\S4method{wavelength}{Speclib,data.frame}(object) <- value

\S4method{wavelength}{Speclib,numeric}(object) <- value

\S4method{fwhm}{Speclib}(object)

\S4method{fwhm}{Speclib,numeric}(object) <- value
}
\arguments{
  \item{object}{
Object of class \code{Speclib}.
}
  \item{value}{
Numeric vector or data.frame containing wavelength values.
}
}
\details{
Wavelength (band center) and full-width-half-max (fwhm) values are given for each spectral band. The wavelength is mandatory for creation of \code{Speclib} and is used within the whole functionality of the package (e.g., smoothSpeclib, spectralResampling, vegindex, nri, plot, mask).
}
\value{
For \code{wavelength<-} and \code{fwhm<-}, the updated object. Otherwise a numeric vector of the wavelength and fwhm-values is returned.
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
