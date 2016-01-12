\name{caret::sbf}
\docType{methods}
\alias{sbf-methods}
\alias{get_sbf}
\alias{sbf,Speclib-method}
\alias{sbf,Nri-method}

\title{Methods for Function \code{sbf}}
\description{
Methods for function \code{sbf} in package \pkg{caret}.
}
\usage{
\S4method{sbf}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{sbf}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_sbf(x)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib} or \code{Nri}. For \code{get_sbf}, \code{x} must be the output of \code{sbf} as \code{Speclib} or \code{Nri}.
}
  \item{y}{
  A numeric or factor vector containing the outcome for each sample. If missing, the response variable set by \code{\link{setResponse}} is used.
}

  \item{cutoff}{
  The cutoff value of the correlation coefficients between response variables.
}
\item{returnData}{
  Logical. If TRUE, the updated object of \code{x} is returned, otherwise only the result of \code{\link{sbf}} is returned.
}
\item{...}{
  Further aruments passed to \code{\link{sbf}}.
}
}

\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{sbf}.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{sbf}}
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

## Selection by filtering
## Note that this may take some time!
sbf_res <- sbf(x)

get_sbf(sbf_res)

plot(get_sbf(sbf_res))
}
}
\keyword{methods}
