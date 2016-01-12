\name{caret::preProcess-methods}
\docType{methods}
\alias{preProcess-methods}
\alias{preProcess,ANY-method}
\alias{preProcess,.CaretHyperspectral-method}
\alias{show,.preProcessHyperspectral-method}
\alias{preProcess-class}
\title{Methods for Function \code{preProcess}}
\description{
Methods for function \code{preProcess} in package \pkg{caret}. The function is mainly internally required.
}
\section{Methods}{
\describe{

\item{\code{signature(x = ".CaretHyperspectral")}}{
  Wrapper method for \code{\link{preProcess}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}}
\keyword{methods}
