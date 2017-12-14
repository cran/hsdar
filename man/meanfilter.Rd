\name{meanfilter}
\alias{meanfilter}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply mean filter
}
\description{
Apply mean filter to spectra. Filter size is passed as number of bands averaged at both sides of the respective band value.
}
\usage{
meanfilter(spectra, p = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spectra}{
Data.frame, matrix or Speclib containing spectra
}
  \item{p}{
Filter size.
}
}

\value{
Filtered matrix or Speclib of same dimension as input matrix/Speclib
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{noiseFiltering}}
}
\examples{
data(spectral_data)

spectra_filtered <- meanfilter(spectral_data, p = 10)
plot(spectra_filtered[1,])
plot(spectral_data[1,], new = FALSE)

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
