\name{speclib}
\docType{methods}
\alias{speclib}
\alias{print,Speclib-method}
\alias{$,Speclib-method}
\alias{[,Speclib,ANY,ANY-method}
\alias{[,Speclib,ANY,ANY,ANY-method}
\alias{show,Speclib-method}
\alias{initialize,Speclib-method}
\alias{is.speclib}
\alias{createspeclib}
\alias{speclib,matrix,numeric-method}
\alias{speclib,SpatialGridDataFrame,numeric-method}
\alias{speclib,numeric,numeric-method}
\alias{speclib,matrix,data.frame-method}
\alias{speclib,SpatialGridDataFrame,data.frame-method}
\alias{speclib,numeric,data.frame-method}
\alias{speclib,matrix,matrix-method}
\alias{speclib,SpatialGridDataFrame,matrix-method}
\alias{speclib,numeric,matrix-method}
\alias{speclib,HyperSpecRaster,ANY-method}
\alias{speclib,RasterBrick,data.frame-method}
\alias{speclib,RasterBrick,matrix-method}
\alias{speclib,RasterBrick,numeric-method}
\alias{speclib,hyperSpec,ANY-method}
\alias{speclib,character,numeric-method}
\alias{ncol,.Spectra-method}
\alias{nrow,.Spectra-method}
\alias{speclib,Speclib,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods to create objects of class Speclib
}
\description{
Methods to create objects of class Speclib from various data types
}
\usage{
\S4method{speclib}{matrix,numeric}(spectra, wavelength, ...)

\S4method{speclib}{SpatialGridDataFrame,numeric}(spectra, wavelength, ...)

\S4method{speclib}{numeric,numeric}(spectra, wavelength, ...)

\S4method{speclib}{matrix,data.frame}(spectra, wavelength, ...)

\S4method{speclib}{SpatialGridDataFrame,data.frame}(spectra, wavelength, ...)

\S4method{speclib}{numeric,data.frame}(spectra, wavelength, ...)

\S4method{speclib}{matrix,matrix}(spectra, wavelength, ...)

\S4method{speclib}{SpatialGridDataFrame,matrix}(spectra, wavelength, ...)

\S4method{speclib}{numeric,matrix}(spectra, wavelength, ...)

\S4method{speclib}{hyperSpec,ANY}(spectra, wavelength, ...)

\S4method{speclib}{character,numeric}(spectra, wavelength, ...)

\S4method{speclib}{Speclib,numeric}(spectra, wavelength, ...)

\S4method{print}{Speclib}(x)

\S4method{show}{Speclib}(object)

createspeclib(spectra, wavelength, fwhm = NULL, SI = NULL, 
        usagehistory = NULL, transformation = NULL, 
        continuousdata = "auto", wlunit = "nm", 
        xlabel = "Wavelength", ylabel = "Reflectance",
        rastermeta = NULL)

is.speclib(x)
}

\arguments{
\item{spectra}{
Data frame, matrix of raster object of class 'RasterBrick' or 'SpatialGridDataFrame' with spectral data. Alternatively, spectra may be the path to a raster file containing hyperspectral data.
}
\item{x,object}{
Object to be converted to or from Speclib. For conversion to Speclib it can be a of class \code{'data frame'}, \code{'matrix'}, \code{'list'} or \code{'character string'}. In the latter case x is interpreted as path to raster object and read by \code{readGDAL}. For conversion from \code{Speclib} the object must be of class \code{Speclib}.
}
\item{wavelength}{
Vector with corresponding wavelength for each band. A matrix or data.frame may be passed giving the upper and lower limit of each band. In this case, the first column is used as lower band limit and the second as upper limit, respectively.
}
\item{fwhm}{
Vector containing full-width-half-max values for each band
}
\item{SI}{
Data frame with supplementary information to each spectrum.
}
\item{transformation}{
Kind of transformation applied to spectral data (character). See \code{\link{transformSpeclib}} for available ones. If \code{transformation = NULL}, no transformation is assumed (default).
}
\item{usagehistory}{
Character string or vector used for history of usage
}
\item{continuousdata}{
Flag indicating if spectra are quasi continuous or discrete sensor spectra
}
\item{wlunit}{
Unit of wavelength in spectra
}
\item{xlabel}{
Label of wavelength data to be used for plots etc.
}
\item{ylabel}{
Label of spectral signal to be used for plots etc.
}
\item{rastermeta}{
List of meta information for SpatialGridDataFrame. If missing, meta data in speclib is used. Use function \code{\link{rastermeta}} to create valid objects.
}
\item{...}{
Further arguments passed to specific (generic) functions or createspeclib
}
}
\details{
See details in \code{\linkS4class{Speclib}}.
}
\value{
An object of class \code{Speclib} containing the following slots is returned:

\itemize{
  \item{wavelength:}{ Vector with wavelength information}
  \item{fwhm:}{ Vector or single numerical value giving the full-width-half-max value(s) for each band.}
  \item{spectra:}{ Object of class '.Spectra' with three slots:
    \itemize{
      \item{fromRaster:}{ logical, indicating if spectral data is read from a RasterBrick-object.}
      \item{spectra_ma:}{ Matrix with ncol = number of bands and nrow = number. Used if fromRaster == FALSE}
      \item{spectra_ra:}{ RasterBrick-object which is used if fromRaster == TRUE.}
    }
    Contains reflectance, transmittance or absorbance values. Handle with function \code{\link[=spectra.Speclib]{spectra}}.}
  \item{SI:}{ Data frame containing additional data to each spectrum. May be used for linear regression etc. Handle with function \code{\link[=SI.speclib]{SI}}.}
  \item{usagehistory:}{ Vector giving information on history of usage of speclib. Handle with function \code{\link{usagehistory}}.}
  \item{rastermeta:}{ List containing meta information to create *Raster objects from Speclib. Handle with function \code{\link{rastermeta}}.}
}
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}, \code{\link[=plot.Speclib]{plot}}, \code{\link{readGDAL}}, \code{\link{mask}},  

\code{\link{idSpeclib}}, \code{\link[=dim.speclib]{dim}}, \code{\link[=spectra.Speclib]{spectra}},

\code{\link[=SI.speclib]{SI}}
}
\examples{
data(spectral_data)
spectra <- spectra(spectral_data)
wavelength <- spectral_data$wavelength

spectra <- speclib(spectra,wavelength)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
