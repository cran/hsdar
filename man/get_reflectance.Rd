\name{get_reflectance}
\alias{get_reflectance.speclib}
\alias{get_reflectance,Speclib-method}

\title{Get reflectance values}
\description{
Returns weighted or unweighted reflectance values at wavelength position.
}

\usage{
\S4method{get_reflectance}{Speclib}(spectra, wavelength, position, weighted = FALSE,
                        ...)

}
\arguments{
  \item{spectra}{
    Object of class \code{Speclib} or data.frame with reflectance values.
  }
  \item{wavelength}{
    Vector with wavelength values.
  }
  \item{position}{ 
    Numeric value passing the position of reflectance values to be returned in dimensions of the wavelength values.
  }
  \item{weighted}{ 
    Logical indicating if reflectance values should be interpolated to fit wavelength position. If \code{FALSE} the reflectance values of nearest neighbour to passed position are returned.
  }
  \item{...}{
    Arguments to be passed to specific functions. For \code{get_reflectance.default} ignored.
  }
}
\value{
  A vector with reflectance values for each spectrum is returned. If position falls outside of spectral range of input values, \code{NA} values are returned.
}
\author{ Lukas Lehnert & Hanna Meyer }
\seealso{
 \code{\link[=spectra.Speclib]{spectra}}
}
\examples{
data(spectral_data)
}
