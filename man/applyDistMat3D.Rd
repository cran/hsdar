\name{apply.DistMat3D}
\alias{apply.DistMat3D}
\alias{apply,DistMat3D-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply function for class DistMat3D
}
\description{
Apply function to values in a 3-D distance matrix. The 3-D distance matrix is an S4-class in \pkg{hsdar} to efficiently store distance values in hyperspectral datasets.
}
\usage{
\S4method{apply}{DistMat3D}(X, MARGIN, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Object of class \code{'\linkS4class{DistMat3D}'}.
}
\item{MARGIN}{
  A vector giving the subscripts (dimensions) of the \code{DistMat3D}-object which the function will be applied over (see examples).
}
  \item{FUN}{
Function to be applied. Matched with \code{\link{match.fun}}.
}
\item{...}{
Further arguments passed to FUN.
}
}

%\details{
%  
%  The specified function is either applied to the distances of all samples (MARGIN = 1) or to all distances for each sample (MARGIN = 3). In the %first case, if \code{X} would be replaced by an array of same dimensions, the return value would be equal if the following code is applied: 
%  
%  \code{apply(X, MARGIN = c(1,2), FUN)}, 
%  
%  where X is an array (see examples).
%}
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

## Part I: Create an object of class DistMat3D
## Calculate all possible NRI - combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Get all NRI-values as numeric vector
nri_values <- as.numeric(t(as.matrix(getNRI(nri_WV, 
                                            getFiniteNri(nri_WV)))))

## Create object of class DistMat3D
dmat <- distMat3D(nri_values, 8, 45)


## Part II: Apply function mean to values in the new object
## Calculate mean value of all samples for all indices 
meanIndexVals <- apply(dmat, MARGIN = 1, FUN = mean)
## Convert to DistMat3D
meanIndexVals <- distMat3D(meanIndexVals, 8, 1)

## Same but for array
nri_WV_dat <- as.array(dmat)
meanIndexVals_arr <- apply(nri_WV_dat, MARGIN = c(1, 2), FUN = mean)
## Convert to DistMat3D
meanIndexVals_arr <- distMat3D(meanIndexVals_arr)

## Test if equal
all(meanIndexVals_arr == meanIndexVals)


## Calculate mean value of all indices wihtin each sample
meanSampleVals <- apply(dmat, MARGIN = 3, FUN = mean)
meanSampleVals_arr <- apply(nri_WV_dat, MARGIN = 3, FUN = mean, 
                            na.rm = TRUE)
## Test if equal
all(meanSampleVals == meanSampleVals_arr)

## User-defined function (in this case the median)
quant <- function(x)
  return(quantile(x,  probs = 0.5))
## Apply user defined function to all samples for all indices 
medianIndexVals <- apply(dmat, MARGIN = 1, FUN = quant)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

