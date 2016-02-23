\name{apply.DistMat3D}
\alias{apply.DistMat3D}
\alias{apply,DistMat3D-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply function for class DistMat3D
}
\description{
Apply function to values in a 3-D distance matrix
}
\usage{
\S4method{apply}{DistMat3D}(X, MARGIN, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Object of class 'DistMat3D'.
}
\item{MARGIN}{
  A vector giving the subscripts (dimensions) of the DistMat3D-object which the function will be applied over (see details).
}
  \item{FUN}{
Function to be applied. Matched with \code{\link{match.fun}}.
}
\item{...}{
Further arguments passed to FUN.
}
}

\details{
  The specified function is either applied to the distances of all samples (MARGIN = 1) or to all distances for each sample (MARGIN = 3). In the first case, if X would be replaced by an array of same dimensions the return value would be equal if the following code is applied: 
  
  \code{apply(X, MARGIN = c(1,2), FUN)}, 
  
  where X is an array (see examples).
}
\value{
Depending on the length of the return value of the specified function, objects of classes numeric or matrix are returned. 
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{apply}}, \code{\link{match.fun}}, \code{\linkS4class{DistMat3D}}
}
\examples{
data(spectral_data)

## Calculate NDVI
ndvi <- nri(spectral_data, b1=800, b2=680)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)
class(nri_WV@nri)

## Calculate mean value of all samples for all indices 
meanIndexVals <- apply(nri_WV@nri, MARGIN = 1, FUN = mean)
meanIndexVals

## Same but for array
nri_WV_dat <- as.array(nri_WV@nri)
meanIndexVals_arr <- apply(nri_WV_dat, MARGIN = c(1, 2), FUN = mean)

meanSampleVals <- apply(nri_WV@nri, MARGIN = 3, FUN = mean)
meanSampleVals_arr <- apply(nri_WV_dat, MARGIN = 3, FUN = mean)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

