\name{spectralInterpolation}
\alias{spectralInterpolation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate spectra
}
\description{
Interpolate spectra to user defined bands. Currently, only a linear interpolation is supported
}
\usage{
  spectralInterpolation(x, sensor) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Speclib}.
}
  \item{sensor}{
    \code{data.frame} containing definition of sensor characteristics. See details section for further information.
}
}
\details{
  The characteristics must be passed as a \code{data.frame} with two columns: first column with lower bounds of channels and second column with upper bounds. Alternatively, the \code{data.frame} may encompass band centre wavelength and full-width-half-maximum values of the sensor. Function will check the kind of data passed by partially matching the names of the data frame: If any column is named \code{"fwhm"} or \code{"center"}, it is assumed that data are band centre and full-width-half-maximum values. 
}

\author{
Lukas Lehnert
}
\value{
  Object of class \code{Speclib} containing the updated version of x.
}
\seealso{
  \code{\link{spectralResampling}}
}

\examples{
## Load example data
data(spectral_data)
## Create sensor featuring 10 times higher spectral resolution
bounds <- seq(min(wavelength(spectral_data)),
              max(wavelength(spectral_data)), 
              length.out = nbands(spectral_data)*10)
sensor <- data.frame(lb = bounds[-1*100 + 1], ub = bounds[-1])
## Interpolate first spectrum
inter <- spectralInterpolation(spectral_data[1,], sensor = sensor)
}
\keyword{ utilities }
