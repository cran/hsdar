\name{cut_specfeat}
\alias{cut_specfeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cut absorption features
}
\description{
Function cuts absorption features to a user-specified range.
}
\usage{
cut_specfeat(x, ..., fnumber, limits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "Specfeat" containing isolated features determined by \code{\link{specfeat}}.
}
  \item{fnumber}{
A vector of the positions of the features in x to be cut.
}
  \item{limits}{
A vector containing the start and end wavelength for each fnumber. 
The corresponding feature will be cut to this specified range.
}
  \item{...}{
Further arguments passed to generic functions. Currently ignored.
}
}
\value{
An object of class \code{\linkS4class{Specfeat}} containing the cut features.
}

\author{
Hanna Meyer and Lukas Lehnert
}

\seealso{
\code{\link{define.features}}, \code{\link{specfeat}}, \code{\linkS4class{Specfeat}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
data(spectral_data)

##Example to cut the features around 450nm and 700nm to a specific range
## Transform speclib
bd <- transformSpeclib(subset(spectral_data, season == "summer"),
                       method = "sh", out = "bd")

## Define features
features <- define.features(bd)

## Convert speclib to specfeat giving center wavelength of features
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Cut 1st and 2nd feature to [310 nm, 560 nm] and [589 nm, 800 nm]
featuresCut <- cut_specfeat(x = featureSelection, fnumber = c(1,2), 
                            limits = c(c(310, 560), c(589, 800)))

## Plot result (1st and 2nd feature)
plot(featuresCut, fnumber = 1:2)
}