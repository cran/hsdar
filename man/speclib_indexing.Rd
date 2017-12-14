\name{Extract Speclib by index}
\alias{[,Speclib,ANY,ANY-method}
\alias{[,Speclib,ANY,ANY,ANY-method}
\alias{[,Speclib-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Indexing Speclib
}
\description{
Access subsets of data in Speclibs both in spectrals and sample dimensions
}
\usage{
\S4method{[}{Speclib}(x, i, j, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Speclib} to be indexed.
}
  \item{i}{
    Samples to be returned.
}
  \item{j}{
    Bands to be returned.
}
\item{...}{
    Further arguements (currently ignored).
}
}
\details{
The first index represents the sample dimension and the second one is the band dimension. If the sample dimension is indexed, care is taken that the SI and the id is indexed as well.
}

\author{
Lukas Lehnert 
}
\value{
  Object of class \code{Speclib} containing the updated version of x.
}
\seealso{
\code{\linkS4class{Speclib}}, \code{\link{subset.speclib}}, \code{\link{SI}}, \code{\link{idSpeclib}}
}

\examples{
data(spectral_data)

## Get the first five spectra
spec_1_5 <- spectral_data[1:5,]
spec_1_5

## Get the first ten bands
spec_1_10 <- spectral_data[,1:10]
spec_1_10

## Get the bands number 20 to 30 for the third and fifth spectra
spec_20_30 <- spectral_data[c(3,5),20:30]
spec_20_30
}
\keyword{ utilities }
