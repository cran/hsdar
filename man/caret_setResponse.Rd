\name{caret::setResponse}
\docType{methods}
\alias{setResponse-methods}
\alias{setResponse}
\alias{setResponse,.CaretHyperspectral,character-method}

\title{Set response variable}
\description{
Set response variable to be used in model-fitting functions of package \pkg{caret}. The response variable must be set upon any model training using a \pkg{hsdar}-object in \pkg{caret}.
}
\usage{
\S4method{setResponse}{.CaretHyperspectral,character}(x, response)
}
\arguments{
  \item{x}{
  Object of one of the following classes: 
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
  \item{response}{
  Character. Name of response variable (from the SI).
}
}

\value{
The updated object.
}

\author{
Lukas Lehnert
}
\seealso{
\code{\link{showCaretParameters}}, \code{\link{setPredictor}}
}
\examples{
\dontrun{
data(spectral_data)

## Set response variable (Chlorophyll content)
spectral_data <- setResponse(spectral_data, "chlorophyll")

## Show caret related parameters stored in the Speclib
showCaretParameters(spectral_data)
}
}
\keyword{methods}
