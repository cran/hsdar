\name{get.response}
\alias{get.response}
\alias{get_RapidEye_response}
% \alias{get_landsat4_response}
% \alias{get_landsat5_response}
% \alias{get_landsat7_response}
% \alias{TerraModis_response}
% \alias{get_quickbird_response}
\alias{get_response_function}
% \alias{get_wv2_4_response}
% \alias{get_wv2_8_response}

\title{
Satellite response functions
}
\description{
Handling satellite sensor response functions
}
\usage{
get.response(sensor, range = NULL, response_function = TRUE, 
             continuousdata = "auto") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensor}{
Name or integer value of satellite sensor. Matched with \code{\link{get.sensor.name}}.
}
  \item{range}{
Vector of length = 2 containing maximum and minimum wavelength to be considered.
}
  \item{response_function}{
If \code{TRUE}, spectral response function else wise Gaussian response function will be returned.
}
  \item{continuousdata}{
Definition if returned \code{\linkS4class{Speclib}} is containing continuous data or not.
}
}
\value{
Object of class \code{\linkS4class{Speclib}} containing spectral response values instead of reflectance value. Spectral response values may be accessed with \code{\link[=spectra.Speclib]{spectra}}.
}
\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.sensor.name}}, \code{\link{get.sensor.characteristics}}, 
\code{\link{get.gaussian.response}}

}
\examples{
## See example in get.sensor.characteristics
}
\keyword{utilities}