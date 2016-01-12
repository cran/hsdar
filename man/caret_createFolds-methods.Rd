\name{caret::createFolds-methods}
\docType{methods}
\alias{createFolds-methods}
\alias{createMultiFolds-methods}
\alias{createFolds,ANY-method}
\alias{createMultiFolds,ANY-method}
\alias{createFolds,.CaretHyperspectral-method}
\alias{createMultiFolds,.CaretHyperspectral-method}
\title{Methods for Function \code{createFolds} and \code{createMultiFolds}}
\description{
Methods for functions \code{createFolds} and \code{createMultiFolds} in package \pkg{caret}
}
\section{Methods}{
\describe{

\item{\code{signature(y = ".CaretHyperspectral")}}{
  Wrapper methods for \code{\link{createFolds}} and \code{\link{createMultiFolds}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}}
\keyword{methods}
