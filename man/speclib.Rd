\encoding{utf-8}
\name{speclib}
\docType{methods}
\alias{speclib}
\alias{print,Speclib-method}
\alias{$,Speclib-method}
\alias{show,Speclib-method}
\alias{initialize,Speclib-method}
\alias{is.speclib}
%\alias{createspeclib}
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
%\alias{speclib,hyperSpec-method}
\alias{speclib,character,numeric-method}
\alias{ncol,.Spectra-method}
\alias{nrow,.Spectra-method}
\alias{speclib,Speclib,numeric-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods to create objects of class Speclib
}
\description{
Methods to create objects of class Speclib from various data sources such as matrixes and raster files (e.g. GeoTiff).
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

%\S4method{speclib}{hyperSpec}(spectra, wavelength, ...)
%
\S4method{speclib}{character,numeric}(spectra, wavelength, ...)

\S4method{speclib}{Speclib,numeric}(spectra, wavelength, ...)

\S4method{print}{Speclib}(x)

\S4method{show}{Speclib}(object)

is.speclib(x)
}
%createspeclib(spectra, wavelength, fwhm = NULL, SI = NULL, 
%        usagehistory = NULL, transformation = NULL, 
%        continuousdata = "auto", wlunit = "nm", 
%        xlabel = "Wavelength", ylabel = "Reflectance",
%        rastermeta = NULL)

\arguments{
\item{spectra}{
Data frame, matrix or raster object of class 'RasterBrick' or 'SpatialGridDataFrame' with spectral data. Alternatively, spectra may be the path to a raster file containing hyperspectral data.
}
\item{x,object}{
Object to be converted to or from Speclib. For conversion to Speclib it can be a of class \code{'data frame'}, \code{'matrix'}, \code{'list'} or \code{'character string'}. In the latter case x is interpreted as path to raster object and read by \code{readGDAL}. For conversion from \code{Speclib} the object must be of class \code{Speclib}.
}
\item{wavelength}{
Vector with corresponding wavelength for each band. A matrix or data.frame may be passed giving the upper and lower limit of each band. In this case, the first column is used as lower band limit and the second as upper limit, respectively. 
}
\item{...}{
Further arguments passed to specific (generic) functions. %or createspeclib
Theey encompass particularly the following additional parameters:
\itemize{
\item{\code{fwhm}:}{
Vector containing full-width-half-max values for each band. Default: NULL
}
\item{\code{SI}:}{
Data frame with supplementary information to each spectrum. Default: NULL
}
\item{\code{transformation}:}{
Kind of transformation applied to spectral data (character). See \code{\link{transformSpeclib}} for available ones. If \code{transformation = NULL}, no transformation is assumed (default).
}
\item{\code{usagehistory}:}{
Character string or vector used for history of usage. Default: NULL
}
\item{\code{continuousdata}:}{
Flag indicating if spectra are quasi continuous or discrete sensor spectra (deprecated). Default: "auto"
}
\item{\code{wlunit}:}{
Unit of wavelength in spectra. Default: "nm". See datails how other units are treated.
}
\item{\code{xlabel}:}{
Label of wavelength data to be used for plots etc. Default: "Wavelength"
}
\item{\code{ylabel}:}{
Label of spectral signal to be used for plots etc. Default: "Reflectance"
}
\item{\code{rastermeta}:}{
List of meta information for SpatialGridDataFrame. If missing, meta data in speclib is used. Use function \code{\link{rastermeta}} to create valid objects. Default: NULL
}
}
}
}
\details{
\subsection{Spectral data}{
  The spectral data (usually reflectance values) are stored in an object of class \code{'.Spectra'}. This object may eiter contain the spectral data as a \code{RasterBrick} or as a \code{matrix} with columns indicating spectral bands and rows different samples, respectively. The Speclib-class provides converting routines to and from \code{RasterBrick}-class allowing to read and write geographic raster data via \code{\link[raster]{brick}}. Since R is in general not intended to be used for VERY large data sets, this functionality should be handled with care. If raster files are large, one should split them in multiple smaller ones and process each of the small files, separately. See the excellent tutorial 'Writing functions for large raster files' available on \url{https://CRAN.R-project.org/package=raster} and section '2.2.2 Speclibs from raster files' in 'hsdar-intro.pdf'.

}
\subsection{Spectral information}{
  Speclib contains wavelength information for each band in spectral data. This information is used for spectral resampling, vegetation indices and plotting etc. Since spectra can be handled either as continuous lines or as discrete values like for satellite bands, spectral information is handled in two principle ways:
  \itemize{
    \item{Continuous spectra:}{ Data of spectrometers or hyperspectral (satellite) sensors. This data is plotted as lines with dotted lines indicating standard deviations by default.}
    \item{Non-continuous spectra:}{ Data of multispectral satellite sensors. Here, data is plotted as solid lines and error bars at the mean position of each waveband indicating standard deviations by default.}
  }
  The kind of data may be chosen by the user by setting the flag \code{"continuousdata"} (\code{attr(x,} \code{"continuousdata")}) or passing \code{continuousdata = TRUE/FALSE}, when initially converting data to \code{\linkS4class{Speclib}}-class. Take care of doing so, because some functions as \code{\link{spectralResampling}} may only work correctly with continuous data!
  
  The internal and recommended wavelength unit is nm. If Speclibs are created with wavelength values in other units than nm as passed by \code{wlunit}-argument, wavelength values are automatically converted to nm. In this case, functions requiring to pass wavelength information (e.g., \code{mask} etc) expect the unit to match the one initially set. The only exception is the Nri-class which always uses and expects nm as unit of passed wavelength values. The following units are automatically detected: \eqn{mu}, \eqn{\mu m}{µm}, \eqn{nm}, \eqn{mm}, \eqn{cm}, \eqn{dm}, \eqn{m}.
}
\subsection{Technical description}{
An object of class \code{Speclib} contains the following slots:

\itemize{
  \item{wavelength:}{ Vector with wavelength information. Always stored in nm.}
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
}
}
}
\value{
An object of class \code{Speclib} containing the following slots is returned:

\itemize{
  \item{wavelength:}{ Vector with wavelength information. Always stored in nm.}
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
