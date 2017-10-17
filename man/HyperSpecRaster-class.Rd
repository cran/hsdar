\name{HyperSpecRaster-class}

\docType{class}

\alias{HyperSpecRaster-class}
\alias{show,HyperSpecRaster-method}

\title{ HyperSpecRaster* class (deprecated)}
\description{
This is a deprecated class. Use \code{\linkS4class{Speclib}}-class instead.
}
\author{
Lukas Lehnert
}
\details{
Extension of *RasterBrick-class with three additional slots:
\describe{
    \item{\code{wavelength}:}{ A numeric vector giving the center wavelength for each band.}
    \item{\code{fwhm} (optional):}{ A numeric vector giving the full-width-half-max values for each band.}
    \item{\code{SI} (optional):}{ A \code{data.frame} containing additional information for each pixel.}
}
The information in the three slots are used for the convertion to \code{\linkS4class{Speclib}}.
}

\seealso{ \code{\link[raster]{brick}},  \code{\linkS4class{Speclib}}}

\keyword{spatial}
\keyword{classes}
