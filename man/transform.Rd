\name{transform_speclib}
\alias{transform_speclib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform spectra
}
\description{
Transform spectra by using convex hull or segmented upper hull 
}
\usage{
  transform_speclib(data, ..., method = "ch", out = "bd")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Speclib to be transformed
}
  \item{method}{
Method to be used. See details section.
}
\item{out}{
Kind of value to be returned. See details section.
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
}
\details{

Function performs a continuum removal transformation by firstly establishing a continuum 
line/hull which connects the local maxima of the reflectance
spectrum. Two kinds of this hull are well established in scientific community: the
convex hull (e.g. Mutanga et al. 2004) and the segmented
hull (e.g. Clark et al. 1987). Both hulls are
established by connecting the local maxima, however, the precondition of the convex
hull is that the resulting continuum line must be convex whereas considering the
segmented hull it might be concave or convex but the algebraic sign of the slope is
not allowed to change from the global maximum of the spectrum downwards to the
sides. In contrast to a convex hull, the segmented hull is able to identify small absorption
features. 

Specify \code{method = "ch"} for the convex hull and \code{method = "sh"} for the segmented hull.
The output might be \code{"raw"}, \code{"bd"} or \code{"ratio"}:
\itemize{
\item{"raw":}{ the continuum line is returned} 
\item{"bd":}{ the spectra are transformed to band depth by
\deqn{BD_\lambda = 1-\frac{R_\lambda}{CV_\lambda},} 
where \eqn{BD} is the band depth, \eqn{R} is the reflectance and \eqn{CV} is the continuum value at
the wavelength \eqn{\lambda}.}
\item{"ratio":}{ the spectra are transformed by 
\deqn{BD_\lambda = \frac{R_\lambda}{CV_\lambda}.}
}
}
In some cases it might be useful to apply \code{\link{smoothSpeclib}} before the transformation if too many small local maxima
are present in the spectra. Anyway, a manual improvement of the continuum line is possible 
using \code{\link{addcp}} and \code{\link{deletecp}}.

}
\value{
If \code{method != "raw"} an object of class \code{\linkS4class{Speclib}} containing transformed spectra is returned. Otherwise the return object will be of class \code{\link{Clman}}.
}
\references{
Clark, R. N., King, T. V. V. and Gorelick, N. S. (1987): Automatic continuum analysis of reflectance spectra. 
Proceedings of the Third Airborne Imaging Spectrometer Data Analysis Workshop, 30. 138-142.

Mutanga, O. and Skidmore, A. K. (2004): Narrow band vegetation indices overcome the saturation problem in biomass estimation.
International Journal of Remote Sensing, 25, 3999-4014.
}
\author{
Hanna Meyer and Lukas Lehnert
}


\seealso{
\code{\link{Clman}}, \code{\link{addcp}}, \code{\link{deletecp}}, \code{\link{checkhull}}
}
\examples{
data(spectral_data)

transformed_spectra <- transform_speclib(spectral_data)

par(mfrow=c(1,2))
plot(spectral_data)
plot(transformed_spectra)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
