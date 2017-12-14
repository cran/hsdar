\name{noiseFiltering}
\alias{noiseFiltering}
\alias{smoothSpeclib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smooth spectra
}
\description{
Smoothing of spectral data by Savitzky-Golay, lowess, spline, mean or user-defined filtering approaches.
}
\usage{
noiseFiltering(x, method = "mean", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{\linkS4class{Speclib}}.
}
  \item{method}{
Character string giving the name of the method to be used. Predefined valid options are "sgolay", "lowess", "spline" and "mean". However, method can also be the (character) name of any other filter function (see examples).
}
  \item{...}{
Further arguments passed to the filter functions. The following arguments are important for the predefined methods:
\itemize{
  \item{\code{sgolay}: }{\code{n} sets the filter length (must be odd).}
  \item{\code{lowess}: }{\code{f} defines the smoother span. This gives the proportion of bands in the spectrum which influence the smooth at each value. Larger values give more smoothness.}
  \item{\code{spline}: }{\code{n} defines at how many  equally spaced points spanning the interval interpolation takes place.}
  \item{\code{mean}: }{\code{p} sets the filter size in number of bands. Note that larger values give more smoothness.}
}
Refer to the links in the details section, and see examples.
}
}
\details{
  Smoothing of spectra by filtering approaches is an essential technique in pre-processing of hyperspectral data with its contiguous spectra. By stepwise fitting of the spectral channels within a defined window size, it is used to minimize the variances caused by intrumental variations or the high noise levels resulting from the very fine wavelength resolution. Therefore, this function allows filtering using four different methods:
  \itemize{
    \item{Savitzky-Golay:}{ Smoothing applying Savitzky-Golay-Filter. See \code{\link{sgolayfilt}} from signal-package for details.}
    \item{Lowess:}{ Smoothing applying lowess-Filter. See \code{\link{lowess}} from stats-package for details.}
    \item{Spline:}{ Smoothing applying spline-Filter. See \code{\link{spline}} from stats-package for details.}
    \item{Mean:}{ Smoothing applying mean-Filter. See \code{\link{meanfilter}} for details.}
  }
}
\value{
Object of class \code{Speclib}.
}
\references{
Tsai, F. & Philpot, W. (1998): Derivative analysis of hyperspectral data. Remote Sensing of Environment 66/1. 41-51.

Vidal, M. & Amigo, J. (2012): Pre-processing of hyperspectral images. Essential steps before image analysis. Chemometrics and Intelligent Laboratory Systems 117. 138-148.
}
\author{
Lukas Lehnert, Wolfgang Obermeier
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sgolayfilt}}, \code{\link{lowess}}, \code{\link{spline}}, \code{\link{meanfilter}}
}
\examples{
data(spectral_data)

## Example of predefined filter functions
## Savitzky-Golay
sgolay <- noiseFiltering(spectral_data, method="sgolay", n=25)

## Spline
spline <- noiseFiltering(spectral_data, method="spline", 
                         n=round(nbands(spectral_data)/10,0))

## Lowess
lowess <- noiseFiltering(spectral_data, method="lowess", f=.01)

## Mean
meanflt <- noiseFiltering(spectral_data, method="mean", p=5)

par(mfrow=c(2,2))
plot(spectral_data, FUN=1, main="Savitzky-Golay")
plot(sgolay, FUN=1, new=FALSE, col="red", lty="dotted")
plot(spectral_data, FUN=1, main="Spline")
plot(spline, FUN=1, new=FALSE, col="red", lty="dotted")
plot(spectral_data, FUN=1, main="Lowess")
plot(lowess, FUN=1, new=FALSE, col="red", lty="dotted")
plot(spectral_data, FUN=1, main="Mean")
plot(meanflt, FUN=1, new=FALSE, col="red", lty="dotted")

## Example of a not predefined filter function (Butterworth filter)
bf <- butter(3, 0.1)
bf_spec <- noiseFiltering(spectral_data, method="filter", filt=bf)
plot(spectral_data, FUN=1, main="Butterworth filter")
plot(bf_spec, FUN=1, new=FALSE, col="red", lty="dotted")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
