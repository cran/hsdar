\name{plot.Speclib}
\alias{plot.Speclib}
\alias{plot,Speclib,ANY-method}
\alias{plot,Clman,ANY-method}
\alias{legendSpeclib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot speclib
}
\description{
Plot \code{Speclib} in a new plot or adding it to an existing plot.
}
\usage{
\S4method{plot}{Speclib,ANY}(x, FUN = NULL, new = TRUE, ...)

\S4method{plot}{Clman,ANY}(x, ispec, subset = NULL, numeratepoints = TRUE, 
    hull.style = NULL, points.style = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
  \item{FUN}{
Name of a function (character) or index or ID of single spectrum to plot (integer).
}
  \item{new}{
If FALSE the plot is added to active existing plot.
}
\item{ispec}{
  Subscript of spectrum to be plotted.
}
  \item{subset}{
Vector of length = 2 containing minimum and maximum wavelength to plot.
}
\item{numeratepoints}{
  Flag if continuum points are numerated and labeled.
}
\item{hull.style}{
List of arguments passed to \code{\link{lines}} to construct the continuum line.
}
\item{points.style}{List of arguments passed to \code{\link{points}} to construct the continuum points. May be \code{NULL} to suppress plotting of fix points.}
  \item{...}{
Further arguments passed to internal plot functions or to \code{\link[=plot.Speclib]{plot}} for objects of class \code{Speclib} and \code{Clman}.
}
}
\details{
The function may work in a couple of modes. The default way is to plot mean values (solid line) of all spectra and the standard deviations within bands. If data is assumed to be continuous the standard deviations are plotted as dashed lines otherwise error bars will indicate standard deviations.

The user has various options to change the way things are looking: With argument \code{FUN} the name of a function, the ID or the index of a certain spectrum may be specified. Note that if \code{FUN} is a function, this function will be applied to all spectra. If function should be applied to a subset of spectra, use function \code{\link[=subset.speclib]{subset}} to define rules excluding certain spectra.

By passing a subset, the user may specify a spectral range to plot. Limits for x- and y-axis will be found automatically or may be passed separately.
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

## Set mask for channel crossing and water absorption bands
mask(spectral_data) <- c(1040, 1060, 1350, 1450)

## Simple example
plot(spectral_data, legend = list(x = "topleft"))

## Example with groups
plot(spectral_data, bygroups = TRUE, legend = list(x = "topleft"))

## Example with function
par(mfrow = c(2,3))
plot(spectral_data, FUN = "min", main = "Minimum of speclib")
plot(spectral_data, FUN = "max", main = "Maximum of speclib")
plot(spectral_data, FUN = "median", main = "Median of speclib")
plot(spectral_data, FUN = "mean", main = "Mean of speclib")
plot(spectral_data, FUN = "var", main = "Variance of speclib")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
