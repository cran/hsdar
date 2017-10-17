\name{caret::sbf}
\docType{methods}
\alias{sbf-methods}
\alias{get_sbf}
\alias{sbf,Speclib-method}
\alias{sbf,Nri-method}
\alias{sbf,Specfeat-method}

\title{Methods for Function \code{sbf}}
\description{
Methods for function \code{sbf} in package \pkg{caret}. Please refer to help pages in the \pkg{caret}-package for further information.
}
\usage{
\S4method{sbf}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{sbf}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{sbf}{Specfeat}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_sbf(sbf_obj)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib}, \code{Nri} or \code{Specfeat}. 
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
  \item{sbf_obj}{
  Object of class \code{Speclib}, \code{Nri} or \code{Specfeat} as output of \code{sbf}-function.
}
}

\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{sbf}. Note that if \code{x} is an object of class \code{Specfeat}, the function returns an object of class \code{Speclib} containing the relevant transformed band values.
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

## Set response variable (Chlorophyll content)
spectral_data <- setResponse(spectral_data, "chlorophyll")

## Set additional predictor variables from the SI
spectral_data <- setPredictor(spectral_data, "season")

## Selection by filtering
## Note that this may take some time!
sbf_res <- sbf(spectral_data)

get_sbf(sbf_res)

plot(get_sbf(sbf_res))
}
}
\keyword{methods}
