\name{response_functions}
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
\docType{data}
\title{
Satellite sensor response functions
}
\description{
Satellite sensor response functions for all sensor channels
}
% \usage{
% data(satellite_response)
% }
\details{
Please do not access this data directly, since it contains only the response values without any spectral information. %To get response functions use function \code{\link{get.response}}, instead.
}
\note{
This data is kindly provided by operators of satellites. See \code{\link{hsdardocs}("Copyright")} for copyright information on spectral response functions.
\itemize{
  \item{Quickbird: }{Copyright by DigitalGlobe, Inc. All Rights Reserved}
  \item{RapidEye: }{Copyright by RapidEye AG}
  \item{WorldView-2: }{Copyright by DigitalGlobe, Inc. All Rights Reserved}
}
}
\format{An object of class 'data.frame'}
\keyword{datasets}

