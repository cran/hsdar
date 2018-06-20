\name{HyperSpecRaster}

\alias{HyperSpecRaster}

\alias{HyperSpecRaster,character,numeric-method}
\alias{HyperSpecRaster,RasterLayer,numeric-method}
\alias{HyperSpecRaster,RasterBrick,numeric-method}
\alias{brick,Speclib,ANY-method}
\alias{HyperSpecRaster,Speclib-method}
\alias{HyperSpecRaster,Speclib,ANY-method}

\alias{writeStart,HyperSpecRaster,character-method}
\alias{writeStart,HyperSpecRaster,Speclib-method}

\alias{getValuesBlock,HyperSpecRaster}


\alias{HyperSpecRaster,HyperSpecRaster-method}
\alias{getValuesBlock,HyperSpecRaster-method}
\alias{writeValues,RasterLayer,Speclib-method}
\alias{writeValues,RasterBrick,Speclib-method}
\alias{writeValues,HyperSpecRaster,Speclib-method}


\title{Handle hyperspectral cubes using raster package (deprecated)}

\description{
The HyperSpecRaster-Class is deprecated. Use \code{\linkS4class{Speclib}} instead.
}
% generic 'HyperSpecRaster' and siglist 'RasterBrick,numeric'
%   generic 'HyperSpecRaster' and siglist 'RasterLayer,numeric'
%   generic 'HyperSpecRaster' and siglist 'character,numeric'

\usage{
\S4method{HyperSpecRaster}{character,numeric}(x, wavelength, fwhm = NULL, SI = NULL, ...)

\S4method{HyperSpecRaster}{RasterLayer,numeric}(x, wavelength, fwhm = NULL, SI = NULL)

\S4method{HyperSpecRaster}{RasterBrick,numeric}(x, wavelength, fwhm = NULL, SI = NULL)

\S4method{HyperSpecRaster}{HyperSpecRaster}(x, wavelength)

\S4method{HyperSpecRaster}{Speclib}(x, nrow, ncol, xmn, xmx, ymn, ymx, crs)

\S4method{writeStart}{HyperSpecRaster,character}(x, filename, ...)

\S4method{getValuesBlock}{HyperSpecRaster}(x, ...) 

\S4method{writeValues}{RasterLayer,Speclib}(x, v, start) 

\S4method{writeValues}{RasterBrick,Speclib}(x, v, start)

\S4method{writeValues}{HyperSpecRaster,Speclib}(x, v, start) 
}

\arguments{
  \item{x}{Raster* object}
  \item{wavelength}{Vector containing wavelength for each band}
  \item{fwhm}{Optional vector containing full-width-half-max values. If length == 1 the same value is assumed for each band. Note that function does not check the integrity of the values}
  \item{SI}{Optional data.frame containing SI data}
  \item{nrow}{Optional. Number of rows in HyperspecRaster. If omitted, function will try to get the information from the SI in Speclib (\code{attr(x, "rastermeta")})}
  \item{ncol}{Optional. Number of colums in HyperspecRaster. See nrow above.}
  \item{xmn}{Optional. Minimum coordiante in x-dimension. See nrow above.}
  \item{xmx}{Optional. Maximum coordiante in x-dimension. See nrow above.}
  \item{ymn}{Optional. Minimum coordiante in y-dimension. See nrow above.}
  \item{ymx}{Optional. Maximum coordiante in y-dimension. See nrow above.}
  \item{crs}{Optional. Object of class \code{'CRS'} giving the coordinate system for HyperspecRaster. See nrow above.}
  \item{...}{Additional arguments as for \code{\link{brick}}}
  \item{filename}{Name of file to create}
  \item{v}{Speclib or matrix of values}
  \item{start}{Integer. Row number (counting starts at 1) from where to start writing \code{v}}
}

\author{
Lukas Lehnert
}

\value{
HyperSpecRaster or RasterBrick
}





\keyword{ spatial }
\keyword{ methods }
