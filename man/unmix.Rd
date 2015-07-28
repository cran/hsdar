\name{unmix}
\alias{unmix}
\title{
Unmix spectra
}
\description{
Unmix spectra or spectra resampled to satellite bands using endmember spectra
}
\usage{
unmix(spectra, endmember, returnHCR = "auto", scale = FALSE, ...)
}
\arguments{
  \item{spectra}{
Input spectra of class 'speclib'
}
  \item{endmember}{
Endmember spectra of class 'speclib'
}
\item{returnHCR}{
Set class of value. If TRUE, value will be of class '\code{HyperSpecRaster}', otherwise a list is returned. If \code{auto}, function will switch to mode depending on input data characteristics.
}
\item{scale}{
Flag to scale spectra to [0,1] if necessary.
}
\item{...}{
Further arguments passed to \code{\link{HyperSpecRaster}} (ignored if returnHCR = FALSE).
}
}
\details{
Linear spectral unmixing is a frequently used method to calculate fractions of land-cover classes (endmembers) within the footprint of pixels. This approach has originally been intended to be used for multispectral satellite images. The basic assumption is that the signal received at the sensor (\eqn{\rho_{mix}}{p_{mix}}) is a linear combination of \eqn{n} pure endmember signals (\eqn{\rho_{i}}{p_i}) and their cover fractions (\eqn{f_{i}}{f_i}):
\deqn{
  \rho_{mix} = \sum^{n}_{i=1} \rho_i f_i,
}{
  p_{mix} = \sum^{n}_{i=1} p_i f_i,
}
where \eqn{f_1, f_2 , ..., f_n >= 0} and \eqn{\sum^{n}_{i=1} f_i = 1} to fulfill two constraints:
\enumerate{
\item{All fractions must be greater or equal 0}
\item{The sum of all fractions must be 1}
}
Since this linear equation system is usually over-determined, a least square solution is performed. The error between the final approximation and the observed pixel vector is returned as vector (\code{error}) in list (\code{returnSpatialGrid = FALSE}) or as last band if \code{returnSpatialGrid = TRUE}.
}
\value{
A list containing the fraction of each endmember in each spectrum and an 
error value giving the euclidean norm of the error vector after least 
square error minimisation.
}
\references{
Sohn, Y. S. & McCoy, R. M. (1997): Mapping desert shrub rangeland using spectral unmixing and modeling spectral mixtures with TM data. Photogrammetric Engineering and Remote Sensing, 63, 707-716
}
\author{
Lukas Lehnert
}
\note{
Unmixing code is based on "i.spec.unmix" for GRASS 5 written by Markus Neteler (1999).
}

\examples{
\dontrun{
data(spectral_data)
data(endmember_spectra)

## Mask channel crossing part (arround 1050 nm) and strong 
## water absorption part (above 1350 nm)
mask(spectral_data) <- c(1045, 1055, 1350, 1706)

## Convert range of reflectance values to [0, 1]
spectra(spectral_data) <- spectra(spectral_data)/100
spectra(endmember_spectra) <- spectra(endmember_spectra)/100

## Perform unmixing for spectra from Kailash-site
unmix_fractions <- unmix(subset(spectral_data, Site == "Kailash"), 
                         subset(endmember_spectra, Site == "Kailash"))

}
}
\keyword{multivariate}
