\name{caret::setPredictor}
\docType{methods}
\alias{setPredictor-methods}
\alias{setPredictor}
\alias{setPredictor,.CaretHyperspectral,character-method}

\title{Set predictor variable(s)}
\description{
Set predictor variable(s) to be used in functions of package \pkg{caret}.
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
  Character vector. Name of additional predictor variable(s) (from the attributes).
}
}

\value{
The updated object.
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

## Set additional predictor variables from the attributes
x <- setPredictor(x, "season")
}
}
\keyword{methods}
