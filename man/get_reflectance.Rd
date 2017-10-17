\name{get_reflectance}
\alias{get_reflectance}


\title{Get reflectance values}
\description{
Returns weighted or unweighted reflectance values at wavelength position.
}

\usage{
get_reflectance(spectra, wavelength, position, weighted = FALSE, ...)

}
\arguments{
  \item{spectra}{
    Object of class \code{Speclib} or data.frame with reflectance values.
  }
  \item{wavelength}{
    Vector with wavelength values. May be missing if spectra is object of class \code{Speclib}.
  }
  \item{position}{ 
    Numeric value passing the position of reflectance values to be returned in dimensions of the wavelength values.
  }
  \item{weighted}{ 
    Logical indicating if reflectance values should be interpolated to fit wavelength position. If \code{FALSE} the reflectance values of nearest neighbour to passed position are returned.
  }
  \item{...}{
    Arguments to be passed to specific functions. Currently ignored.
  }
}
\value{
  A vector with reflectance values for each spectrum is returned. If position falls outside of spectral range of input values, \code{NA} values are returned.
}
\author{ Lukas Lehnert \& Hanna Meyer }
\seealso{
 \code{\link[=spectra.Speclib]{spectra}}
}
\examples{
data(spectral_data)

## Simulate multispectral sensor encompassing two bands 
## to show effect of weighted and unweighted modes
spectral_data_res <- spectralResampling(spectral_data, 
  sensor = data.frame(lb = c(400, 600), ub = c(500, 700)))

## Compare reflectance at 520 nm (in between both bands to 
## show the difference between weighted and unweighted modes)
weighted_reflectance   <- get_reflectance(spectral_data_res, 
                                          520, weighted = TRUE)
unweighted_reflectance <- get_reflectance(spectral_data_res, 
                                          520, weighted = FALSE)

## Plot result
plot(weighted_reflectance, unweighted_reflectance, 
     ylab = "Reflectance at 520 nm (unweighted)", 
     xlab = "Reflectance at 520 nm (weighted)")
}
