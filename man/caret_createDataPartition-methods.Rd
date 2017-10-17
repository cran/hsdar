\name{caret::createDataPartition-methods}
\docType{methods}
\alias{createDataPartition-methods}
\alias{createDataPartition,ANY-method}
\alias{createDataPartition,.CaretHyperspectral-method}
\title{Methods for Function \code{createDataPartition}}
\description{
 Methods for function \code{createDataPartition} in package \pkg{caret}. Please refer to help pages in the \pkg{caret}-package for further information.
}
\section{Methods}{
\describe{

\item{\code{signature(y = ".CaretHyperspectral")}}{
  Wrapper method for \code{\link{createDataPartition}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}}
\keyword{methods}
