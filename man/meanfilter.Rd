\name{meanfilter}
\alias{meanfilter}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply mean filter
}
\description{
Apply mean filter to data frame with spectra as rows and bands as columns. Filter size is passed as number of bands averaged at both sites of the respective band value.
}
\usage{
meanfilter(spectra, p = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spectra}{
Data frame containing spectra
}
  \item{p}{
Filter size.
}
}

\value{
Filtered data frame of same dimension as input data frame
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{smoothSpeclib}}
}
\examples{
data(spectral_data)

spectra_filtered <- meanfilter(spectra(spectral_data), p = 10)
spectra(spectral_data) <- spectra_filtered 

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
