\name{caret::rfe}
\docType{methods}
\alias{rfe-methods}
\alias{get_rfe}
\alias{rfe,Speclib-method}
\alias{rfe,Nri-method}

\title{Methods for Function \code{rfe}}
\description{
Methods for function \code{rfe} in package \pkg{caret}.
}
\usage{
\S4method{rfe}{Speclib}(x, y, cutoff = 0.95, returnData = TRUE, ...)

\S4method{rfe}{Nri}(x, y, cutoff = 0.95, returnData = TRUE, ...)

get_rfe(x)
}
\arguments{
  \item{x}{
  Object of class \code{Speclib} or \code{Nri}. For \code{get_rfe}, \code{x} must be the output of \code{rfe} as \code{Speclib} or \code{Nri}.
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
If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{rfe}.
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

## Use subset of data
x <- spectral_data[c(1:31),] 

## Set response variable (Chlorophyll content)
x <- setResponse(x, "chlorophyll")

## Set additional predictor variables from the attributes
x <- setPredictor(x, "season")

## Recursive feature selection
## Note that this may take some time!
rfe_res <- rfe(x)

get_rfe(rfe_res)

plot(get_rfe(rfe_res))
}
}
\keyword{methods}
