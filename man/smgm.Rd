\name{smgm}
\alias{smgm}
\title{
SMGM
}
\description{
Calculate Gaussian model on soil spectra
}
\usage{
smgm(x, percentage = TRUE, gridsize = 50) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class Speclib.
}
  \item{percentage}{
    Flag if spectra in x are in range [0, 100]. If FALSE, the spectra are scaled to [0,100].
}
  \item{gridsize}{
    Size of the grid used to perform least squares approximation.
}
}
\author{
Lukas Lehnert
}
\details{
 The algorithm fits a Gaussian function to the continuum points of the spectra in the spectral region between approx. 1500 to 2500 nm. The continuum points are derived constructing the convex hull of the spectra (see \code{\link{transformSpeclib}}). The Gaussian function requires three parameter: (1) the mean values which is set to the water fundamental of 2800 nm, (2) the absorption depth at 2800 nm, and (3) the distance to the inflection point of the function. The latter two parameters are iteratively chosen using a grid search. The mesh size of the grid can be adjusted with the \code{gridsize} parameter. Note that the function requires the spectral reflectance values to be in interval [0, 100].
}
\value{
  Object of class \code{Speclib} containing the fitted Gaussian spectra and the parameters derived from the Gaussian curve. The three parameters (absorption depth, R0; distance to the inflection point, sigma; area between the curve and 100 \% reflectance, area) are stored in the SI of the new Speclib. Additionally, the function returns the final root mean square error of the Gaussian fit.
}
\references{
Whiting, M. L., Li, L. and Ustin, S. L. (2004): Predicting water content using Gaussian model on soil spectra. Remote Sensing of Environment, 89, 535-552.
}
\note{
The code is based on the IDL functions written by  Michael L. Whiting.
}
\seealso{
\code{\link{soilindex}}, \linkS4class{Speclib}
}

\examples{
## Use PROSAIL to simulate spectra with different soil moisture content
Spektr.lib <- smoothSpeclib(PROSAIL(parameterList = data.frame(psoil = seq(0,1,0.1), LAI = 0)))

smgm_val <- smgm(Spektr.lib)

for (i in 1:nspectra(smgm_val))
  plot(smgm_val, FUN = i, new = i==1, col = i)

SI(smgm_val)
}
