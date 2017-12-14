\name{cut_specfeat}
\alias{cut_specfeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cut absorption features
}
\description{
Function cuts absorption features to a user-specified range. Since features may differ among spectra, it might be important to cut the features to specific wavelengths ranges.

}
\usage{
cut_specfeat(x, ..., fnumber, limits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{Specfeat} containing isolated features determined by \code{\link{specfeat}}.
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


\details{
The typical workflow to obtain feature properties is to first calculate the band 
depth \code{\link{transformSpeclib}}, then isolate the absorption features 
\code{\link{specfeat}}. Optionally, \code{\link{cut_specfeat}} allows to 
cut the features at specified wavelengths. Finally use \code{\link{feature_properties}}
to retrieve characteristics of the features.


}
\value{
An object of class \code{\linkS4class{Specfeat}} containing the cut features.
}

\author{
Hanna Meyer and Lukas Lehnert
}

\seealso{
\code{\link{specfeat}}, \code{\linkS4class{Specfeat}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
data(spectral_data)

##Example to cut the features around 450nm and 700nm to a specific range
## Transform speclib
bd <- transformSpeclib(subset(spectral_data, season == "summer"),
                       method = "sh", out = "bd")

## Convert speclib to specfeat giving center wavelength of features
featureSelection <- specfeat(bd, c(450,700,1200,1500))

## Cut 1st and 2nd feature to [310 nm, 560 nm] and [589 nm, 800 nm]
featuresCut <- cut_specfeat(x = featureSelection, fnumber = c(1,2), 
                            limits = c(c(310, 560), c(589, 800)))

## Plot result (1st and 2nd feature)
plot(featuresCut, fnumber = 1:2)
}