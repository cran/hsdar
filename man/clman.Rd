\name{Clman}

\docType{class}
\alias{Clman}
\alias{plot,Clman-method}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
* Clman class
}
\description{
Class to store and handle manual continuum lines
}
\usage{
Clman(wavelength, cp, hull, spectra, outdatedhull = NULL, mask = NULL)

\S4method{plot}{Clman}(x, ispec, subset = NULL, numeratepoints = TRUE, ...)
}
\arguments{

\item{wavelength}{
Vector with corresponding wavelength for each band.
}
\item{cp}{
Data frame or matrix containing fix points. Fix points have numbers greater than 0, all other bands are 0.
}
\item{hull}{
Data frame or matrix containing linear hull.
}
\item{spectra}{
Data frame, matrix of raster object of class 'SpatialGridDataFrame' with spectral data.
}
\item{outdatedhull}{
Data frame or matrix containing hull of step before for undo porposes.
}
\item{mask}{
Data frame with masked parts in the spectra. See \code{\link{mask}}.
}
\item{x}{
Object of class \code{clman}.
}
\item{ispec}{
Name or index of spectrum to be plotted.
}
\item{subset}{
Lower and upper spectral limits used for plot.
}
\item{numeratepoints}{
Flag if points should be numerated in plot.
}
\item{...}{
Further arguments passed to plot.default.
}
}
\value{
Object of class \code{\linkS4class{Clman}}.
}

\author{
Lukas Lehnert and Hanna Meyer
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{transform_speclib}}
}
\examples{
## Model spectra using PROSAIL
parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
spec <- PROSAIL(parameterList=parameter)

## Transform spectra
spec_clman <- transform_speclib(spec, method = "sh", out = "raw")

## Plot clman
plot(spec_clman, ispec = 1, subset = c(400, 1000))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{aplot}
