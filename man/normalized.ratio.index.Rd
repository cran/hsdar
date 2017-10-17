\name{nri}
\alias{nri}
%\alias{print.nri}
%\alias{as.matrix.nri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalised ratio index
}
\description{
Calculate normalised ratio index (nri) for a single given band combination or for all possible band combinations. Calculating nri is a frequently used method to standardize reflectance values and to find relationships between properties of the objects and their spectral data.
}
\usage{
nri(x, b1, b2, recursive = FALSE, bywavelength = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of class \code{Speclib} or of class \code{Nri} for print and as.matrix methods.
}
  \item{b1}{
Band 1 given as band number or wavelength.
}
  \item{b2}{
Band 2 given as band number or wavelength.
}
  \item{recursive}{
If TRUE indices for all possible band combinations are calculated. If FALSE, only a single nri for the given bands in \code{b1} and \code{b2} is calculated.
}
  \item{bywavelength}{
Flag to determine if b1 and b2 are band number (bywavelength = FALSE) or wavelength (bywavelength = TRUE) values.
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
%  \item{named_matrix}{
%Flag if column names should be set to band names and row names to ids of spectra.
%}

}
\details{
Function performs the following calculation:
\deqn{nri_{B1,~B2}=\frac{R_{B1}-R_{B2}}{R_{B1}-R_{B2}};}
with \eqn{R} being reflectance values at wavelength \eqn{B1} and \eqn{B2}, respectively.

If recursive = TRUE, all possible band combinations are calculated.
}
\value{
If recursive = FALSE, a data frame with index values is returned. Otherwise result is an object of class \code{\linkS4class{Nri}}. See \code{\link{glm.nri}} for applying a generalised linear model to an array of normalised ratio indices.
}

\references{
Sims, D.A.; Gamon, J.A. (2002). Relationships between leaf pigment content and spectral reflectance across a wide range of species, leaf structures and developmental stages. Remote Sensing of Environment: 81/2, 337 - 354.  
  
Thenkabail, P.S.; Smith, R.B.; Pauw, E.D. (2000). Hyperspectral vegetation indices and their relationships with agricultural crop characteristics. Remote Sensing of Environment: 71/2, 158 - 182.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{glm.nri}}, \code{\link{glm}}, \code{\linkS4class{Speclib}}, \code{\linkS4class{Nri}}
}
\examples{
data(spectral_data)

## Calculate NDVI
ndvi <- nri(spectral_data, b1=800, b2=680)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)
nri_WV
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
