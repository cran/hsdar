\name{hsdar_parallel}
\alias{hsdar_parallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hsdar_parallel
}
\description{
Get all functions which support parallel execution
}
\usage{
hsdar_parallel() 
}
%- maybe also 'usage' for other objects documented here.
\details{
  Parallel execution is performed via the \pkg{foreach}-package. Care is taken that a function will never run in parallel if the calling function is already using multicore processing.
}

\author{
Lukas Lehnert
}
\value{
  Vector containing supported function names
}
% \seealso{
% \code{\link{foreach}}
% }

\examples{
\dontrun{
data(spectral_data)
## Load library
library(doMC)
## Register number of workers
registerDoMC(3)

supported_functions <- hsdar_parallel()
supported_functions

## Transform speclib using 3 cores
bd <- transformSpeclib(spectral_data)
}
}
\keyword{ utilities }
