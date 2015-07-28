\name{Raster-methods}

\alias{Raster-methods}

\alias{extract,Speclib,ANY-method}
\alias{writeRaster,Speclib,character-method}

\title{Rasterbased methods for spectra}

\description{
Methods to manipulate and save spectra in Speclibs stored as RasterBrick
}

\usage{
\S4method{extract}{Speclib,ANY}(x, y, ...)

\S4method{writeRaster}{Speclib,character}(x, filename, ...)
}

\arguments{
  \item{x}{Speclib with RasterBrick-object for spectra}
  \item{y}{Object of any valid type to define area to extract}
  \item{filename}{Output filename}
  \item{...}{Additionaly arguments passed to basic funtions in the raster-package}
}

\author{
Lukas Lehnert
}

\value{
Speclib
}

\keyword{ spatial }
\keyword{ methods }