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
## Use PROSAIL to generate some vegetation spectra with different LAI
parameter <- data.frame(LAI = seq(0, 1, 0.01))
spectral_data <- PROSAIL(parameterList = parameter)

## Get endmember spectra
## Retrieve all available spectra
avl <- USGS_get_available_files()

## Download all spectra matching "grass-fescue"
grass_spectra <- USGS_retrieve_files(avl = avl, pattern = "grass-fescue")
limestone <- USGS_retrieve_files(avl = avl, pattern = "limestone")

## Integrate all spectra to Quickbird
grass_spectra_qb <- spectralResampling(grass_spectra[1,], "Quickbird")
limestone_qb <- spectralResampling(limestone, "Quickbird")
spectral_data_qb <- spectralResampling(spectral_data, "Quickbird")


em <- speclib(spectra = rbind(spectra(grass_spectra_qb), 
                              spectra(limestone_qb))/100,
              wavelength = wavelength(limestone_qb))

## Unmix
unmix_res <- unmix(spectral_data_qb, em)

unmix_res

plot(unmix_res$fractions[1,] ~ SI(spectral_data_qb)$LAI, type = "l",
     xlab = "LAI", ylab = "Unmixed fraction of vegetation")
}
}
\keyword{multivariate}
