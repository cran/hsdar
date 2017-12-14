\name{Raster-methods}
\alias{Raster-methods}
% \alias{extract,Speclib,ANY-method}
\alias{extract,Speclib-method}
\alias{writeRaster,Speclib,character-method}
\alias{plotRGB,Speclib-method}
\alias{brick,Speclib-method}

\title{Rasterbased methods for spectra}

\description{
Methods to manipulate, save, convert and plot spectra in Speclibs stored as RasterBrick
}

\usage{
\S4method{extract}{Speclib}(x, y, ...)

\S4method{writeRaster}{Speclib,character}(x, filename, ...)

\S4method{plotRGB}{Speclib}(x, ...)

\S4method{brick}{Speclib}(x, ...)
}

\arguments{
  \item{x}{Speclib with RasterBrick-object for spectra}
  \item{y}{Object of any valid type to define area to extract}
  \item{filename}{Output filename}
  \item{...}{Additionaly arguments passed to basic funtions in the raster-package}
}


\details{
  For \code{extract}, a \code{Speclib} is returned containing the data of \code{y} in the \code{SI}. Note that if \code{y} is a buffer, spatial lines or spatial polygon object, the respective data in \code{y} is copied for each spectrum so that the length of the \code{SI} equals the number of spectra.
  
  For \code{writeRaster}, the Speclib is returned which is written to file. Please note that data in the SI and the wavelength information cannot be stored in a raster file at present. Therefore, it should be considered to store the entire Speclib as R-data file using the \code{\link{save}}-function in R.
  
  Note for function \code{brick} that by default the values of the internal brick in the Speclib are copied to the new object. However, new brick objects with differing dimensions, bands etc. may be created if \code{values == FALSE} is passed as additional arguement.
}
\value{
\code{Speclib} for \code{extract} and \code{writeRaster}. Object of class \code{Brick} for \code{brick}.
}
\author{
Lukas Lehnert
}

\keyword{ spatial }
\keyword{ methods }