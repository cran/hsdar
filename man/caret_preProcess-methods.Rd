\name{caret::preProcess-methods}
\docType{methods}
\alias{preProcess-methods}
\alias{preProcess,ANY-method}
\alias{preProcess,.CaretHyperspectral-method}
\alias{show,.preProcessHyperspectral-method}
\alias{preProcess-class}
\title{Methods for Function \code{preProcess}}
\description{
Methods for function \code{preProcess} in package \pkg{caret}. The function is mainly internally required, but can be also used to transform the reflectance values and the SI e.g., by centering, scaling etc. 
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
