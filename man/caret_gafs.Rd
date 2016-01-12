\name{caret::gafs}
\docType{methods}
\alias{gafs-methods}
\alias{get_gafs}
\alias{gafs,Speclib-method}
\alias{gafs,Nri-method}

\title{Methods for Function \code{gafs}}
\description{
Methods for function \code{gafs} in package \pkg{caret}.
}
\usage{
\S4method{gafs}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{gafs}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_gafs(x)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib} or \code{Nri}. For \code{get_gafs}, \code{x} must be the output of \code{gafs} as \code{Speclib} or \code{Nri}.
}
  \item{y}{
  A numeric or factor vector containing the outcome for each sample. If missing, the response variable set by \code{\link{setResponse}} is used.
}

  \item{cutoff}{
  The cutoff value of the correlation coefficients between response variables.
}
\item{returnData}{
  Logical. If TRUE, the updated object of \code{x} is returned, otherwise only the result of \code{\link{gafs}} is returned.
}
\item{...}{
  Further aruments passed to \code{\link{gafs}}.
}
}

\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{gafs}.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{gafs}}
}
\examples{
\dontrun{
data(spectral_data)

## Use subset of data
x <- spectral_data[c(1:31),] 

## Set response variable (Chlorophyll content)
x <- setResponse(x, "chlorophyll")

## Set additional predictor variables from the attributes
x <- setPredictor(x, "season")

## Feature selection using genetic algorithms
## Note that this may take some time!
gafs_res <- gafs(x)

get_gafs(gafs_res)
}
}
\keyword{methods}
