\name{Speclib-class}

\docType{class}
\alias{Speclib-class}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
* Speclib class
}
\description{
Class to store and handle hyperspectral data in R
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
  
  The unit of spectral data must be set initially, when converting data to speclib. Note that the package currently supports only "nm" as unit. This is particularly important for function like \code{\link{vegindex}}, which need to get correct bands out of the spectral data.
}
\subsection{Technical description}{
An object of class \code{Speclib} contains the following slots:

\itemize{
  \item{wavelength:}{ Vector with wavelength information.}
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
\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in hsdar.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[=plot.Speclib]{plot}}, \code{\link{readGDAL}}, \code{\link{mask}}, \code{\link{idSpeclib}}, 

\code{\link[=dim.speclib]{dim}}, \code{\link[=spectra.Speclib]{spectra}}, \code{\link[=SI.speclib]{SI}}
}
\keyword{classes}
