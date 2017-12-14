\name{hsdar_parallel}
\alias{hsdar_parallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hsdar_parallel
}
\description{
Get all functions which support parallel execution. Currently, the parallel backend functions in \pkg{doMPI} and \pkg{doMC} are supported.
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
supported_functions <- hsdar_parallel()
supported_functions

data(spectral_data)

## Example for Windows and other systems where doMPI is available
## Load library
library(doMPI)
## Register number of workers
cl <- startMPIcluster(count = 3)
registerDoMPI(cl)

## Transform speclib using 3 cores
bd <- transformSpeclib(spectral_data)

## Close the cluster (important to get rid of processes)
closeCluster(cl)

## Example for Linux and other systems where doMC is available
## Load library
library(doMC)
## Register number of workers
registerDoMC(3)

## Transform speclib using 3 cores
bd <- transformSpeclib(spectral_data)
}
}
\keyword{ utilities }
