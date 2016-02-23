\name{bdri}
\alias{bdri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Band depth ratio indices
}
\description{
Calculate band depth ratio indices for objects of class \code{Specfeat}.
}
\usage{
bdri(x, fnumber, index = "ndbi") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Specfeat}.
}
  \item{fnumber}{
    Integer. Index of feature to modify.
}
  \item{index}{
    Method to be applied. Currently, \code{"bdr"}, \code{"ndbi"} and \code{"bna"} are available.
}
}
\details{
  Method \code{"bdr"} calculates the normalised band depth ratio as 
  \deqn{bdr=\frac{BD}{Dc},}
  with \eqn{BD} is the band depth calculated by \code{\link{transformSpeclib}} 
  and Dc is the maximum band depth called band centre.
  Method \code{"ndbi"} calculates the the normalised band depth index as
  \deqn{ndbi= \frac{BD-Dc}{BD+Dc}.}
  Method \code{"bna"} calculates the band depth normalised to band area as
   \deqn{bna=\frac{BD}{Da},}
    where \eqn{Da} is the area of the absorption feature (see 
   \code{\link{feature_properties}}). For further information see Mutanga and Skidmore (2004).
}
\references{
Mutanga, O. and Skidmore, A. (2004): Hyperspectral band depth analysis for a better estimation of grass biomass (\var{Cenchrus ciliaris}) measured under controlled laboratory conditions. International Journal of applied Earth Observation and Geoinformation, 5, 87-96
}
\author{
Lukas Lehnert and Hanna Meyer
}
\value{
  Object of class \code{specfeat} containing the updated version of x.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{define.features}}, \code{\link{specfeat}}
}

\examples{
% \dontrun{
data(spectral_data)

## Transform speclib
bd <- transformSpeclib(subset(spectral_data, season == "summer"),
                       method = "sh", out = "bd")

## Define features automatically
features <- define.features(bd)

## Isolate the features around 450nm, 700nm, 1200nm and 1500nm and 
## convert to specfeat.
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Plot features
plot(featureSelection,1:4)

## Calculate normalized band depth index for first feature
featureSelection_bdri <- bdri(featureSelection, 1, index = "ndbi")

## Plot result
plot(featureSelection_bdri)
% }
}
\keyword{ utilities }
