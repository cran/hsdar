\name{feature_properties}
\alias{feature_properties}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of properties of features
}
\description{
Function calculates feature properties such as the area, the position of the maximum and several other parameters.
}
\usage{
feature_properties(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class 'specfeat'
}
}
\details{
The function calculates several parameters:
\itemize{
  \item{area: }{The feature area is calculated by 
\deqn{area_{F_{i}}=\sum^{max(\lambda)}_{k=min(\lambda)} BD\lambda,}
with \eqn{area_{F_{i}}} is the area of the feature i, \eqn{min(\lambda)} is the minimum wavelength of the spectrum, 
\eqn{max(\lambda)} is the maximum wavelength of the spectrum and \eqn{BD} is the band depth.}
}
}

\value{
A matrix containing feature area values.
}

\author{
Hanna Meyer \& Lukas Lehnert
}

\seealso{
\code{\link{define.features}}, \code{\link{specfeat}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
% \dontrun{
data(spectral_data)

## Example calculating the areas of the features around 450nm, 
## 700nm, 1200nm and 1500nm.
bd <- transform_speclib(subset(spectral_data, Site == "Namco"),
                        method = "sh", out = "bd")

## Define features
features <- define.features(bd)

## Convert speclib to specfeat giving center wavelength of features
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Calculate properties of features
featureProp <- feature_properties(featureSelection)
% }
}
\keyword{multivariate}
