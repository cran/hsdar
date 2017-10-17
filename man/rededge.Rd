\name{rededge}
\alias{rededge}
\title{
Red edge parameter
}
\description{
Derive red edge parameters from hyperspectral data
}
\usage{
rededge(x, smooth = TRUE, round = FALSE, ...)
}
\arguments{
  \item{x}{
    List of class \code{Speclib}
  }
  \item{smooth}{
    Logical indicating if spectral data should be smoothed. See details section.
  }
  \item{round}{
    Logical indicating if resulting wavelength position should be rounded.
  }
  \item{...}{
    Further arguments passed to \code{\link{derivative.speclib}}
  }
}
\details{
Shape and location of the red edge are commonly described by four parameters: 
\itemize{
\item{\eqn{R0}: }{minimum reflectance in the red spectrum} 
\item{\eqn{\lambda 0}{l0}: }{wavelength of the minimum reflectance} 
\item{\eqn{\lambda p}{lp}: }{inflection point} 
\item{\eqn{Rs}: }{shoulder wavelength} 
}

The red edge parameters are calculated as proposed in Bach (1995) from the spectral area between 550 and 900 nm.
\eqn{\lambda 0}{l0} is calculated as the last root before the maximum value of the 2nd derivation. 
The minimum reflectance is the reflectance at (\eqn{\lambda 0}{l0}). 
The inflection point is the root of the 2nd derivative function between the maximum value and 
the minimum value. The shoulder wavelength is the first root beyond the minimum value of the 2nd derivation.

Optional smoothing is performed with 

\code{\link{smoothSpeclib}(x, method = "spline", n = round(nbands(x)/10,0))}

prior to all calculations. Note that reflectance values returned by the \code{rededge}-function are original values and not the smoothed reflectances. This would not be the case, if already smoothed reflectance values are passed to \code{rededge}-function.
}
\value{
A data frame containing parameters for each spectrum.
}
\references{
Bach, H. (1995): Die Bestimmung hydrologischer und landwirtschaftlicher Oberflaechenparameter aus hyperspektralen Fernerkundungsdaten. 
Muenchner Geographische Abhandlungen Reihe B, Band B21.

}
\author{
Hanna Meyer
}

\seealso{
\code{\link{vegindex}}, \code{\link{derivative.speclib}}, \code{\link{smoothSpeclib}}
}
\examples{
data(spectral_data)
rd <- rededge(spectral_data)
boxplot(rd$R0 ~ SI(spectral_data)$season, ylab = "R0")
}
\keyword{multivariate}
