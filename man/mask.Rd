\name{mask}
\alias{mask}
\alias{mask<-}
\alias{maskSpeclib}
\alias{mask,Speclib-method}
\alias{mask<-,Speclib,data.frame-method}
\alias{mask<-,Speclib,list-method}
\alias{mask<-,Speclib,numeric-method}
\alias{mask<-,Speclib,matrix-method}
\alias{interpolate.mask}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mask spectra
}
\description{
Returning and setting mask of spectra in \code{Speclib}. \code{interpolate.mask} linearly interpolates masked parts in spectra.
}
\usage{
\S4method{mask}{Speclib}(object)
\S4method{mask}{Speclib,data.frame}(object) <- value
\S4method{mask}{Speclib,list}(object) <- value
\S4method{mask}{Speclib,numeric}(object) <- value

## Linear interpolation of masked parts
interpolate.mask(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{Speclib}.
}
  \item{value}{
Numeric vector, data frame or list giving the mask boundaries in wavelength units. See details section.
}
}
\details{
  Value may be an object of class vector, data frame or list. Data frames must contain 2 columns with the first column giving the lower (lb) and the second the upper boundary (ub) of the wavelength ranges to be masked. List must have two items consisting of vectors of length = 2. The first entry is used as lower and the second as upper boundary value. Vectors must contain corresponding lower and upper boundary values consecutively. The masked wavelength range(s) as defined by the lower and upper boundaries are excluded from the object of class \code{Speclib}.
  
  Interpolation of masked parts is mainly intended for internal use. Interpolation is only possible if mask does not exceed spectral range of \code{Speclib}.
}
\value{
For \code{mask<-}, the updated object. Otherwise a data frame giving the mask boundaries.

\code{interpolate.mask} returns a new object of class \code{Speclib}.
}

\author{
Lukas Lehnert and Hanna Meyer
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

mask(spectral_data) ## NULL


## Mask from vector
spectral_data_ve <- spectral_data
mask(spectral_data_ve) <- c(1040,1060,1300,1450)
mask(spectral_data_ve)


## Mask from data frame
spectral_data_df <- spectral_data
mask(spectral_data_df) <- data.frame(lb=c(1040,1300),ub=c(1060,1450))
mask(spectral_data_df)


## Mask from list
spectral_data_li <- spectral_data
mask(spectral_data_li) <- list(lb=c(1040,1300),ub=c(1060,1450))
mask(spectral_data_li)

## Linear interpolation
plot(spectral_data)
plot(interpolate.mask(spectral_data_li), new=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
