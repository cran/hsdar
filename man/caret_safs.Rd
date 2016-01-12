\name{caret::safs}
\docType{methods}
\alias{safs-methods}
\alias{get_safs}
\alias{safs,Speclib-method}
\alias{safs,Nri-method}

\title{Methods for Function \code{safs}}
\description{
Methods for function \code{safs} in package \pkg{caret}.
}
\usage{
\S4method{safs}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{safs}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_safs(x)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib} or \code{Nri}. For \code{get_safs}, \code{x} must be the output of \code{safs} as \code{Speclib} or \code{Nri}.
}
  \item{y}{
  A numeric or factor vector containing the outcome for each sample. If missing, the response variable set by \code{\link{setResponse}} is used.
}

  \item{cutoff}{
  The cutoff value of the correlation coefficients between response variables.
}
\item{returnData}{
  Logical. If TRUE, the updated object of \code{x} is returned, otherwise only the result of \code{\link{safs}} is returned.
}
\item{...}{
  Further aruments passed to \code{\link{safs}}.
}
}

\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{safs}.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{safs}}
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

## Supervised feature selection using simulated annealing
## Note that this may take some time!
safs_res <- safs(x)

get_safs(safs_res)

plot(get_safs(safs_res))
}
}
\keyword{methods}
