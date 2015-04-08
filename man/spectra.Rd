\name{spectra}
\alias{spectra.Speclib}
\alias{spectra}
\alias{spectra<-}
\alias{spectra,Speclib-method}
% \alias{spectra<-,Speclib-method}
\alias{spectra<-,Speclib,data.frame-method}
\alias{spectra<-,Speclib,matrix-method}
\alias{spectra<-,Speclib,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling spectra
}
\description{
Returning and setting spectra in Speclib
}
\usage{
\S4method{spectra}{Speclib}(object, ...)

\S4method{spectra}{Speclib,data.frame}(object) <- value

\S4method{spectra}{Speclib,matrix}(object) <- value

\S4method{spectra}{Speclib,numeric}(object) <- value
}
\arguments{
  \item{object}{
Object of class \code{Speclib}.
}
\item{...}{
Passed to internal function. Currently only one parameter is accepted: \code{return_names}: Logical indicating, if names of columns and rows should be set to \code{\link{bandnames}} and \code{\link{idSpeclib}}.
}
  \item{value}{
Matrix containing spectral values. Columns are band values and rows are spectra.
}
}
\details{
  For \code{spectra<-}, the function does not check if dimensions of spectra match dimensions of Speclib. Additionally, no conversion into \code{matrix} is performed! If spectra are not correctly stored, errors in other functions may arise. Thus check always carefully, if spectra are modified by hand.
}
\value{
For \code{spectra<-}, the updated object. Otherwise a data frame of the spectra in x is returned.
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

## Manual plot of the first spectrum
plot(spectral_data$wavelength, spectra(spectral_data)[1,], type="l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
