\name{spectra}
\alias{spectra.Speclib}
\alias{spectra}
\alias{spectra<-}
\alias{spectra,Speclib-method}
\alias{spectra<-,Speclib,data.frame-method}
\alias{spectra<-,Speclib,matrix-method}
\alias{spectra<-,Speclib,numeric-method}
\alias{spectra<-,Speclib,RasterBrick-method}
\alias{[,.Spectra,ANY,ANY,ANY-method}
\alias{show,.Spectra-method}
\alias{print,.Spectra-method}


% Die Raster-methods sollten noch separat ausgegliedert werden
\alias{cellFromCol,Speclib-method}
\alias{cellFromLine,Speclib-method}
\alias{cellFromPolygon,Speclib-method}
\alias{cellFromRow,Speclib-method}
\alias{cellFromRowCol,Speclib-method}
\alias{cellFromRowColCombine,Speclib-method}
\alias{cellFromXY,Speclib-method}
\alias{colFromX,Speclib-method}
\alias{fourCellsFromXY,Speclib-method}
\alias{rowFromY,Speclib-method}
\alias{readAll,Speclib-method}


\title{
Handling spectra
}
\description{
Returning and setting spectra in Speclib
}
\usage{
\S4method{spectra}{Speclib}(object, i, j, ...)

\S4method{spectra}{Speclib,data.frame}(object) <- value

\S4method{spectra}{Speclib,matrix}(object) <- value

\S4method{spectra}{Speclib,numeric}(object) <- value

\S4method{spectra}{Speclib,RasterBrick}(object) <- value
}
\arguments{
  \item{object}{
Object of class \code{Speclib}.
}
\item{i}{
Index of spectra to return. If missing all spectra are returned.
}
\item{j}{
Index of bands to return. If missing all bands are returned.
}
\item{...}{
Passed to internal function. Currently only one parameter is accepted: \code{return_names}: Logical indicating, if names of columns and rows should be set to \code{\link{bandnames}} and \code{\link{idSpeclib}}.
}
  \item{value}{
Matrix or RasterBrick-object containing spectral values. If value is a matrix, columns are band values and rows are spectra.
}
}
\details{
  For \code{spectra<-}, the function does not check if dimensions of spectra match dimensions of Speclib. Additionally, no conversion into \code{matrix} is performed! If spectra are not correctly stored, errors in other functions may arise. Thus check always carefully, if spectra are modified by hand.
}
\value{
For \code{spectra<-}, the updated object. Otherwise a matrix of the spectra in x is returned.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

## Manual plot of the first spectrum
plot(wavelength(spectral_data), spectra(spectral_data)[1,], type="l")
}
\keyword{utilities}
