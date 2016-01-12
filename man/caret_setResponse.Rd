\name{caret::setResponse}
\docType{methods}
\alias{setResponse-methods}
\alias{setResponse}
\alias{setResponse,.CaretHyperspectral,character-method}

\title{Set response variable}
\description{
Set response variable to be used in functions of package \pkg{caret}.
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
  Character. Name of response variable (from the attributes).
}
}

\value{
The updated object.
}

\author{
Lukas Lehnert
}

\examples{
\dontrun{
data(spectral_data)

## Use subset of data
x <- spectral_data[c(1:31),] 

## Set response variable (Percentage of green vegetation)
x <- setResponse(x, "PV")
}
}
\keyword{methods}
