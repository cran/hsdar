\name{spectralResampling}
\alias{spectralResampling}
\alias{spectral.resampling}
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
If \code{TRUE}, the spectral response function of the sensor is used for integration, if \code{FALSE} a Gaussian distribution is assumed and if \code{NA} the mean value of \code{spectra[min(ch):max(ch)]} is calculated.
}
}
\details{
The characteristics of (satellite) sensor to integrate spectra can be chosen from a list of already implemented sensors. See  \code{\link{get.sensor.characteristics}} for available sensors. 

Otherwise the characteristics can be passed as a \code{data.frame} with two columns: first column with lower bounds of channels and second column with upper bounds. Alternatively, the \code{data.frame} may encompass band centre wavelength and full-width-half-maximum values of the sensor. Function will check the kind of data passed by partially matching the names of the data frame: If any column is named \code{"fwhm"} or \code{"center"}, it is assumed that data are band centre and full-width-half-maximum values. 

If sensor characteristics are defined manually, a Gaussian response is always assumed.
}
\value{
Object of class \code{Speclib}
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{get.sensor.characteristics}}, \code{\link{get.gaussian.response}}%%, \code{\link{speclib}},
}
\examples{
% \dontrun{
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
% }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
