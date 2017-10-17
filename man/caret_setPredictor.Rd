\name{caret::setPredictor}
\docType{methods}
\alias{setPredictor-methods}
\alias{setPredictor}
\alias{setPredictor,.CaretHyperspectral,character-method}

\title{Set predictor variable(s)}
\description{
Set predictor variable(s) to be used in model-fitting functions of package \pkg{caret}. This function can be used to define additional predictor variables stored in the SI of a \code{Speclib}- or \code{Nri}-object. If the passed object is of class \code{Nri}, By default, all Nri-indices (if the passed object is of class \code{Nri}) or all bands (if the passed object is of class \code{Speclib}) are used as predictors.
}
\usage{
\S4method{setPredictor}{.CaretHyperspectral,character}(x, predictor)
}
\arguments{
  \item{x}{
  Object of one of the following classes: 
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
  \item{predictor}{
  Character vector. Name of additional predictor variable(s) (from the SI).
}
}

\value{
The updated object.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{showCaretParameters}}, \code{\link{setResponse}}
}
\examples{
\dontrun{
data(spectral_data)

## Set "season" as additional predictor variable from the SI
spectral_data <- setPredictor(spectral_data, "season")

## Show caret related parameters stored in the Speclib
showCaretParameters(spectral_data)
}
}
\keyword{methods}
