\name{smoothSpeclib}
\alias{smoothSpeclib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smooth spectra
}
\description{
Smooth spectra using Savitzky-Golay filtering, lowess-, spline-functions or mean filter.
}
\usage{
smoothSpeclib(x, method = "mean", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{\linkS4class{Speclib}}.
}
  \item{method}{
Character string giving the method to be used. Predefined valid options are "sgolay", "lowess", "spline" and "mean". However, method can also be the (character) name of any other filter function (see examples).
}
  \item{...}{
Further arguments passed to filter functions. See examples.
}
}
\details{
  This function allows filtering using four different methods:
  \itemize{
    \item{Savitzky-Golay:}{ Smoothing applying Savitzky-Golay-Filter. See \code{\link{sgolayfilt}} for details.}
    \item{Lowess:}{ Smoothing applying lowess-Filter. See \code{\link{lowess}} for details.}
    \item{Spline:}{ Smoothing applying spline-Filter. See \code{\link{spline}} for details.}
    \item{Mean:}{ Smoothing applying mean-Filter. See \code{\link{meanfilter}} for details.}
  }
}
\value{
Object of class \code{Speclib}.
}
\references{
Tsai, F. & Philpot, W. (1998): Derivative analysis of hyperspectral data. Remote Sensing of Environment 66/1. 41-51.
}
\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sgolayfilt}}, \code{\link{lowess}}, \code{\link{spline}}, \code{\link{meanfilter}}
}
\examples{
data(spectral_data)

## Example of predefined filter functions
## Savitzky-Golay
sgolay <- smoothSpeclib(spectral_data, method="sgolay", n=25)

## Spline
spline <- smoothSpeclib(spectral_data, method="spline", 
                         n=round(nbands(spectral_data)/10,0))

## Lowess
lowess <- smoothSpeclib(spectral_data, method="lowess", f=.01)

## Mean
meanflt <- smoothSpeclib(spectral_data, method="mean", p=5)

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
bf_spec <- smoothSpeclib(spectral_data, method="filter", filt=bf)
plot(spectral_data, FUN=1, main="Butterworth filter")
plot(bf_spec, FUN=1, new=FALSE, col="red", lty="dotted")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
