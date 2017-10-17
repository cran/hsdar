\name{caret::showCaretParameters}
\alias{showCaretParameters}
\alias{showCaretParameters,.CaretHyperspectral-method}
\title{Show caret related parameters}
\description{
Show caret related parameters in objects of classes \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }. Several parameters such as predictor and response variables are internally stored and used for model training and validation in the \pkg{caret}-package.
}
\usage{
showCaretParameters(x)
}
\arguments{
  \item{x}{
  Object of one of the following classes: 
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{sbf}}
}

