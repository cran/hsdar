\name{spectralResampling}
\alias{spectralResampling}
\alias{spectral.resampling}
\alias{response_functions}
\alias{Landsat_4_response}
\alias{Landsat_5_response}
\alias{Landsat_7_response}
\alias{Landsat_8_response}
\alias{Sentinel2A_response}
% \alias{TerraModis_response}
\alias{Quickbird_response}
\alias{RapidEye_response}
\alias{WV_2_8_response}
\alias{response_functions}
\alias{get_landsat4_response}
\alias{get_landsat5_response}
\alias{get_landsat7_response}
\alias{get_landsat8_response}
\alias{get_quickbird_response}
\alias{get_wv2_4_response}
\alias{get_wv2_8_response}
\alias{get_sentinel2_response}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spectral resampling
}
\description{
Resample spectra to (satellite) sensors
}
\usage{
spectralResampling(x, sensor, rm.NA = TRUE, continuousdata = "auto",
                   response_function = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}. Data to be spectrally resampled.
}
  \item{sensor}{
Character or \code{data.frame} containing definition of sensor characteristics. See details section for further information.
}
\item{rm.NA}{
If \code{TRUE}, channels which are not covered by input data wavelength are removed
}
\item{continuousdata}{
Definition if returned \code{\linkS4class{Speclib}} is containing continuous data or not.
}
\item{response_function}{
If \code{TRUE}, the spectral response function of the sensor is used for integration, if \code{FALSE} a Gaussian distribution is assumed and if \code{NA} the mean value of \code{spectra[min(ch):max(ch)]} is calculated. If \code{response_function} is an object of class \code{Speclib} the function assumes that the spectra in the object are spectral response values. In this case the wavelength dimension determines the spectral response values for the respective wavelength and the sample dimension separates between the different bands. Note that if \code{response_function} is an object of class \code{Speclib}, \code{sensor} may be missing. In this case the function calculates the central wavelength and the fwhm-values from the spectral response functions.
}
}
\details{
The characteristics of (satellite) sensor to integrate spectra can be chosen from a list of already implemented sensors. See  \code{\link{get.sensor.characteristics}} for available sensors. 

Otherwise the characteristics can be passed as a \code{data.frame} with two columns: first column with lower bounds of channels and second column with upper bounds. Alternatively, the \code{data.frame} may encompass band centre wavelength and full-width-half-maximum values of the sensor. Function will check the kind of data passed by partially matching the names of the data frame: If any column is named \code{"fwhm"} or \code{"center"}, it is assumed that data are band centre and full-width-half-maximum values. 

The third option is to use a \code{Speclib} containing the spectral response values instead of reflectances. In this case, the \code{sensor}-argument may be missing and the function automatically determines the sensor's central wavelength and the fwhm-values based on the spectral response values. See examples.

If sensor characteristics are defined manually and no \code{Speclib} with spectral response values is passed, a Gaussian response is assumed.
}
\value{
Object of class \code{Speclib}
}

\author{
Lukas Lehnert
}
\note{
The spectral response functions are kindly provided by the operators of the satellites. See \code{\link{hsdardocs}("Copyright")} for copyright information on spectral response functions.
\itemize{
  \item{Quickbird: }{Copyright by DigitalGlobe, Inc. All Rights Reserved}
  \item{RapidEye: }{Copyright by RapidEye AG}
  \item{WorldView-2: }{Copyright by DigitalGlobe, Inc. All Rights Reserved}
}
}

\seealso{
\code{\link{get.sensor.characteristics}}, \code{\link{get.gaussian.response}}%%, \code{\link{speclib}},
}
\examples{
## Load example data  
data(spectral_data)

## Resample to RapidEye
data_RE <- spectralResampling(spectral_data, "RapidEye", 
                              response_function = TRUE)

## Plot resampled spectra
plot(data_RE)

## Compare different methods of spectral resampling
par(mfrow=c(1,3))
ga <- spectralResampling(spectral_data, "RapidEye", 
                         response_function = FALSE)
plot(ga)
re <- spectralResampling(spectral_data, "RapidEye", 
                         response_function = TRUE)
plot(re)
no <- spectralResampling(spectral_data, "RapidEye", 
                         response_function = NA)
plot(no)

## Usage of Speclib with spectral response values
## Define 3 bands (RGB)
center <- c(460, 530, 600)
fwhm   <- 70
wl     <- c(310:750)

## Create spectral response with gaussian density function
response <- speclib(t(sapply(center, function(center, wl, fwhm)
{
  a <- dnorm(wl, mean = center, sd = fwhm/2)
  a <- (a-min(a))/(max(a) - min(a))
  return(a)
}, wl, fwhm)), wl)

## Plot response functions
for (i in 1:3)
  plot(response[i,], new = i == 1, col = c("blue", "green", "red")[i])
  
## Perform resampling
rgb_data <- spectralResampling(spectral_data, response_function = response)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
