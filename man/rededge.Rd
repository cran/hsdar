\name{rededge}
\alias{rededge}
\title{
Red edge parameter
}
\description{
Derive red edge parameters from hyperspectral data. Red edge is the sharp increase of reflectance values in the near infrared.
}
\usage{
rededge(x)
}
\arguments{
  \item{x}{
    List of class \code{Speclib}
  }
}
\details{
Shape and location of the red edge are commonly described by four parameters: 
\itemize{
\item{\eqn{\lambda 0}{l0}: }{wavelength of the minimum reflectance in the red spectrum} 
\item{\eqn{\lambda p}{lp}: }{wavelength of the inflection point} 
\item{\eqn{\lambda s}{ls}: }{wavelength of the reflectance shoulder} 
\item{\eqn{R0}: }{reflectance at l0} 
\item{\eqn{Rp}: }{Reflectance at lp} 
\item{\eqn{Rs}: }{Reflectance at ls} 
}

The red edge parameters are calculated as proposed in Bach (1995) from the spectral area between 550 and 900 nm.
\eqn{\lambda 0}{l0} is calculated as the last root before the maximum value of the 2nd derivation. 
The minimum reflectance is the reflectance at (\eqn{\lambda 0}{l0}). 
The inflection point is the root of the 2nd derivative function between the maximum value and 
the minimum value. The shoulder wavelength is the first root beyond the minimum value of the 2nd derivation. The following figure shows the locaqtion of the red edge parameters in an example second derivation and reflectance spectrum.

\if{html}{\figure{rededge.png}{width="35\%" alt="Figure: rededge.png"}}
\if{latex}{\figure{rededge.pdf}{options: width=10cm}}

}
\value{
A data frame containing parameters for each spectrum.
}
\references{
Bach, H. (1995): Die Bestimmung hydrologischer und landwirtschaftlicher Oberflaechenparameter aus hyperspektralen Fernerkundungsdaten. 
Muenchner Geographische Abhandlungen Reihe B, Band B21.


Hamilton, R. M., Foster, R. E., Gibb, T. J., Johannsen, C. J. and Santini, J. B. (2009): Pre-visible detection of Grub Feeding in turfgrass using remote sensing. Photogrammetric Engineering & Remote Sensing, 75, 179-192.
}
\author{
Hanna Meyer
}

\seealso{
\code{\link{vegindex}}, \code{\link{derivative.speclib}}, \code{\link{noiseFiltering}}
}
\examples{
# compare R0 for spectra taken in different seasons
data(spectral_data)
rd <- rededge(spectral_data)
boxplot(rd$R0 ~ SI(spectral_data)$season, ylab = "R0")

# visualize red edge parameter of one spectrum
plot(spectral_data[1,],xlim=c(500,900),ylim=c(0,50))
plot(spectral_data[1,],xlim=c(500,900),ylim=c(0,50))
x <- c(rd$l0[1], rd$lp[1], rd$ls[1])
y <- c(rd$R0[1], rd$Rp[1], rd$Rs[1])
points(x, y)
text(x, y, c("l0", "lp", "ls"), pos = 3, offset = 1)
}
\keyword{multivariate}
