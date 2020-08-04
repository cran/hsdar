\name{Boruta::Boruta}
\docType{methods}
\alias{Boruta-methods}
\alias{Boruta,Speclib-method}
\alias{Boruta,Nri-method}
\alias{Boruta,Specfeat-method}
\alias{get_Boruta}

\title{Methods for Function \code{Boruta}}
\description{
  Methods for function \code{Boruta} in package \pkg{Boruta}. Please refer to help pages in the \pkg{Boruta}-package for further information.
}
\usage{
\S4method{Boruta}{Speclib}(x, y, ..., returnData = TRUE, includeTentative = FALSE,
                           na.rm = FALSE)

\S4method{Boruta}{Nri}(x, y, ..., returnData = TRUE, includeTentative = FALSE,
                       na.rm = FALSE)

\S4method{Boruta}{Specfeat}(x, y, ..., returnData = TRUE, includeTentative = FALSE,
                            na.rm = FALSE)

get_Boruta(x)
}
\arguments{
  \item{x}{
    Object of class \code{Speclib}, \code{Nri} or \code{Specfeat}. For \code{get_Boruta}, \code{x} must be the output of \code{Boruta} as \code{Speclib} or \code{Nri}.
}
  \item{y}{
  A numeric or factor vector containing the outcome for each sample. If missing, the response variable set by \code{\link{setResponse}} is used.
}


\item{returnData}{
  Logical. If TRUE, the updated object of \code{x} is returned, otherwise only the result of \code{\link{Boruta}} is returned.
}
\item{includeTentative}{
  Logical. If TRUE, the tentative variables are kept and returned in the \code{Speclib}-object.}
  \item{na.rm}{
    Logical. If TRUE, all variables are excluded which contain at least one non-finite value.}
\item{...}{
  Further arguments passed to \code{\link{Boruta}}.
}
}

\value{
  If \code{returnData == TRUE}, an object of class \code{Speclib} or \code{Nri}, otherwise an object of class \code{Boruta}. Note that if \code{x} is an object of class \code{Specfeat}, the function returns an object of class \code{Speclib} containing the relevant transformed band values.
}

\author{
Lukas Lehnert
}

\seealso{
  \code{\link{rfe}}, \code{\link{gafs}}
}
\examples{
\dontrun{
data(spectral_data)

## Set response variable (Chlorophyll content)
spectral_data <- setResponse(spectral_data, "chlorophyll")

## Set additional predictor variables from the SI
spectral_data <- setPredictor(spectral_data, "season")

## Run Boruta
## Note that this may take some time!
bor_res <- Boruta(spectral_data)

get_Boruta(bor_res)
plot(get_Boruta(bor_res))
}
}
\keyword{methods}
