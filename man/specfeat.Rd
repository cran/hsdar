\name{specfeat}
\alias{specfeat}
\alias{plot,Specfeat-method}
\alias{[,Specfeat,ANY,ANY,ANY-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to isolate spectral features
}
\description{
Function isolates specified absorption features previously identified by \code{\link{define.features}}.
}
\usage{
specfeat(x, FWL)

\S4method{plot}{Specfeat}(x, fnumber = 1, stylebysubset = NULL, changecol = TRUE, 
     changetype = FALSE, autolegend = TRUE, new = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib} containing the band depth or ratio transformed reflectance spectra with additional information on feature limits calculated by \code{\link{define.features}}. For plot this must be object of class \code{specfeat}.
}
  \item{FWL}{
A vector containing one wavelength per feature to be isolated, e.g. the major absorption features.
Features which include these specified wavelengths will be isolated.
}
  \item{fnumber}{
Index of feature(s) to be plotted.
}
  \item{stylebysubset}{
Name of variable to be used as grouping factor. May be selected from SI table, groups or subgroups and must be convertible to \code{factors}.
}
  \item{changecol}{
Flag, if line colour should be varied among groups
}
  \item{changetype}{
Flag, if line styles should be varied among groups
}
  \item{autolegend}{
Flag if, legend is printed automatically.
}
  \item{new}{
Create new plot or add data to existing one.
}
  \item{...}{
Further arguments passed to plot function.
}
}
\value{
An object of class \code{Specfeat} containing the isolated features.
}

\author{
Hanna Meyer and Lukas Lehnert
}

\seealso{
\code{\link{define.features}}, \code{\link{cut_specfeat}}, \linkS4class{Specfeat}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
% \dontrun{
data(spectral_data)

## Transform speclib
bd <- transformSpeclib(spectral_data, method = "sh", out = "bd")

## Define features automatically
features <- define.features(bd)

##Example to isolate the features around 450nm, 700nm, 1200nm and 1500nm.
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Plot features
plot(featureSelection, fnumber = 1:4)

## Advanced plotting example
plot(featureSelection, fnumber = 1:4, stylebysubset = "season")

plot(featureSelection, fnumber = 1:4, stylebysubset = "season", 
     changecol = FALSE, changetype = TRUE)
% }
}
\keyword{classes}
\keyword{aplot}