\name{caret::rfe}
\docType{methods}
\alias{rfe-methods}
\alias{get_rfe}
\alias{rfe,Speclib-method}
\alias{rfe,Nri-method}
\alias{rfe,Specfeat-method}

\title{Methods for Function \code{rfe}}
\description{
Methods for function \code{rfe} in package \pkg{caret}. Please refer to help pages in the \pkg{caret}-package for further information.
}
\usage{
\S4method{rfe}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{rfe}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{rfe}{Specfeat}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_rfe(x)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib}, \code{Nri} or \code{Specfeat}. For \code{get_rfe}, \code{x} must be the output of \code{rfe} as \code{Speclib} or \code{Nri}.
}
  \item{y}{
  A numeric or factor vector containing the outcome for each sample. If missing, the response variable set by \code{\link{setResponse}} is used.
}

  \item{cutoff}{
  The cutoff value of the correlation coefficients between response variables.
}
\item{returnData}{
  Logical. If TRUE, the updated object of \code{x} is returned, otherwise only the result of \code{\link{rfe}} is returned.
}
\item{...}{
  Further aruments passed to \code{\link{rfe}}.
}
}

\value{
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{rfe}. Note that if \code{x} is an object of class \code{Specfeat}, the function returns an object of class \code{Speclib} containing the relevant transformed band values.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{rfe}}
}
\examples{
\dontrun{
data(spectral_data)

## Set response variable (Chlorophyll content)
spectral_data <- setResponse(spectral_data, "chlorophyll")

## Set additional predictor variables from the SI
spectral_data <- setPredictor(spectral_data, "season")

## Recursive feature selection
## Note that this may take some time!
rfe_res <- rfe(spectral_data)

get_rfe(rfe_res)

plot(get_rfe(rfe_res))
}
}
\keyword{methods}
