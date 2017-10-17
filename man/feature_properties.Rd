\name{feature_properties}
\alias{feature_properties}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of properties of features
}
\description{
Function to calculate feature properties such as the area, the position of the maximum and several other parameters. This function can only be used for spectral data transformed using any kind of continuum removal (see \code{\link{transformSpeclib}}).
}
\usage{
feature_properties(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Specfeat}
}
}
\details{
The function calculates several parameters:
\itemize{
  \item{\emph{area}: }{The feature area is calculated by 
    \deqn{area_{F_{i}}=\sum^{max(\lambda)}_{k=min(\lambda)} BD\lambda,}
    with \eqn{area_{F_{i}}} is the area of the feature i, \eqn{min(\lambda)} is the minimum wavelength of the spectrum, 
    \eqn{max(\lambda)} is the maximum wavelength of the spectrum and \eqn{BD} is the band depth.
  }
  \item{\emph{max}: }{Wavelength position of the maximum value observed in the feature.}
  \item{Parameters based on half-max values: }{
    \itemize{
      \item{\emph{lo} and \emph{up}: }{Wavelength position of the \emph{lo}wer and \emph{up}per half-max value.}
      \item{\emph{width}: }{Difference between wavelength positions of \emph{up}per and \emph{lo}wer half-max values.}        
      \item{\emph{gauss_lo}: }{Similarity of the Gauss distribution function and the feature values between the \emph{lo}wer half-max and the \emph{max}imum position. As similarity measurement, the root mean square error is calculated.}
      \item{\emph{gauss_up}: }{Same as above but for feature values between the \emph{max}imum position and the \emph{up}per half-max.}
    }
  }
}
}

\value{
An object of class \code{Specfeat} containing the properties as (part of the) SI table.
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
bd <- transformSpeclib(subset(spectral_data, season == "summer"),
                       method = "sh", out = "bd")

## Define features
features <- define.features(bd)

## Convert speclib to specfeat giving center wavelength of features
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Calculate properties of features
featureProp <- feature_properties(featureSelection)

## See resulting feature property variables
head(SI(featureProp))
% }
}
\keyword{multivariate}
