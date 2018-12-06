\name{specfeat}
\alias{specfeat}
\alias{[,Specfeat,ANY,ANY,ANY-method}
\alias{n_features}
\alias{n_features,Specfeat-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to isolate absorption features
}
\description{
Function isolates absorption features from band depth or ratio transformed reflectance spectra.
}
\usage{
specfeat(x, FWL, tol = 1.0e-7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib} containing the band depth or ratio transformed reflectance spectra.
}
  \item{FWL}{
A vector containing one wavelength included in each feature to be isolated, e.g. the major absorption features.
Features which include these specified wavelengths will be isolated.
}
  \item{tol}{
The tolerance of the band depth which defines a wavelength as a start or end point of a feature. 
Usually a band depth of 0 or a ratio of 1 indicates feature limits, however, 
better results are achieved if slightly deviating values are tolerated.
}
}

\details{
A feature is defined as the part of the spectrum between two fix points in the transformed spectra (band depth values of 0). This function separates features at wavelengths of interest according to this rule.
Hence it allows a subsequent characterization of the features of interest, e.g. via 
\code{\link{feature_properties}} or visual inspection via \code{\link{plot.Specfeat}}.
The typical workflow to obtain feature properties is to first calculate 
the band depth \code{\link{transformSpeclib}}, then isolate the absorption features 
\code{\link{specfeat}}. Optionally, \code{\link{cut_specfeat}} allows to cut the features at specified wavelengths. Finally use \code{\link{feature_properties}} to retrieve characteristics of the features.
}

\value{
An object of class \code{Specfeat} containing the isolated features.
}

\author{
Hanna Meyer and Lukas Lehnert
}

\seealso{
\code{\link{transformSpeclib}}, \code{\link{cut_specfeat}}, \linkS4class{Specfeat}, \code{\link{plot.Specfeat}},
\code{\link{feature_properties}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
% \dontrun{
data(spectral_data)

## Transform speclib
bd <- transformSpeclib(spectral_data, method = "sh", out = "bd")

##Example to isolate the features around 450nm, 700nm, 1200nm and 1500nm.
featureSelection <- specfeat(bd, c(450,700,1200,1500))

## Plot features
plot(featureSelection)

## Advanced plotting example
plot(featureSelection, 1:2, stylebysubset = "season")

plot(featureSelection, 1:2, stylebysubset = "season", changecol = FALSE, 
     changetype = TRUE)
% }
}
\keyword{classes}
\keyword{aplot}