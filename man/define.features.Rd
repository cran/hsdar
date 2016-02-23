\name{define.features}
\alias{define.features}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Definition of absorption features
}
\description{
Function sets the spectral range of absorption features.
}
\usage{
define.features(x, tol = 1.0e-7, FWL = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib} containing the band depth or ratio transformed reflectance spectra. 
}
  \item{tol}{
The tolerance of the band depth which defines a wavelength as a start or end point of a feature. 
Usually a band depth of 0 or a ratio of 1 indicates feature limits, however, 
better results are achieved if slightly deviating values are tolerated.
}
  \item{FWL}{
Optional. If passed, result is directly converted into \code{\linkS4class{Specfeat}}. A vector containing one wavelength per feature to be isolated, e.g. the major absorption features. Features which include these specified wavelengths will be isolated.
}
}
\details{
Absorption features are defined as the area between local maxima in the reflectance spectra. 
This function adds the information of the feature limits to the \code{Speclib}. 
Thus, it is a pre-processing step to isolate features.
}
\value{
The updated \code{\linkS4class{Speclib}} containing additional information about the feature limits. If \code{FWL} is not \code{NULL}, result will be of class \code{\linkS4class{Specfeat}}.
}

\author{
Hanna Meyer and Lukas Lehnert
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{specfeat}}, \code{\linkS4class{Specfeat}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
% \dontrun{
data(spectral_data)

##Example to define feature limits
bd <- transformSpeclib(subset(spectral_data, season == "summer"), 
                       method = "sh", out = "bd")

## Define features
features <- define.features(bd)
% }
}

